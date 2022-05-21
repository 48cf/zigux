const logger = std.log.scoped(.scheduler);

const root = @import("root");
const std = @import("std");

const arch = @import("arch.zig");
const interrupts = @import("interrupts.zig");
const process = @import("process.zig");
const phys = @import("phys.zig");
const per_cpu = @import("per_cpu.zig");
const virt = @import("virt.zig");
const utils = @import("utils.zig");
const vfs = @import("vfs.zig");

const IrqSpinlock = @import("irq_lock.zig").IrqSpinlock;

const StackHelper = struct {
    rsp: u64,

    fn init(rsp: u64) StackHelper {
        return .{
            .rsp = rsp,
        };
    }

    fn write(self: *StackHelper, bytes: []const u8) void {
        const stack = @intToPtr([*]u8, self.rsp - bytes.len)[0..bytes.len];

        std.mem.copy(u8, stack, bytes);
        self.rsp -= bytes.len;
    }

    fn writeInt(self: *StackHelper, comptime T: type, value: T) void {
        self.write(&std.mem.toBytes(value));
    }

    fn writeString(self: *StackHelper, string: []const u8) void {
        self.writeInt(u8, 0);
        self.write(string);
    }
};

const Shebang = struct {
    path: []const u8,
    arg: []const u8,
};

fn parseShebang(file: *vfs.VNode) !Shebang {
    var path = std.ArrayListUnmanaged(u8){};
    var arg = std.ArrayListUnmanaged(u8){};

    var offset: usize = 2;
    var char: u8 = 0;

    _ = try file.read(@ptrCast([*]u8, &char)[0..1], offset);

    if (char == ' ') {
        offset += 1;
    }

    while (true) {
        _ = try file.read(@ptrCast([*]u8, &char)[0..1], offset);

        offset += 1;

        switch (char) {
            ' ' => break,
            '\n' => return Shebang{ .path = path.items, .arg = "" },
            else => try path.append(root.allocator, char),
        }
    }

    while (true) {
        _ = try file.read(@ptrCast([*]u8, &char)[0..1], offset);

        offset += 1;

        switch (char) {
            ' ', '\n' => break,
            else => try arg.append(root.allocator, char),
        }
    }

    return Shebang{ .path = path.items, .arg = arg.items };
}

pub const Thread = struct {
    tid: u64,
    parent: *process.Process,
    regs: interrupts.InterruptFrame = std.mem.zeroes(interrupts.InterruptFrame),
    node: std.TailQueue(void).Node = undefined,

    pub fn exec(
        self: *Thread,
        file: *vfs.VNode,
        argv: []const []const u8,
        envp: []const []const u8,
    ) !void {
        var shebang_sig = [2]u8{ 0, 0 };

        _ = try file.read(&shebang_sig, 0);

        var file_to_load = file;
        var final_argv = argv;

        if (std.mem.eql(u8, &shebang_sig, "#!")) {
            var shebang = try parseShebang(file);
            var argv_list = std.ArrayListUnmanaged([]const u8){};

            try argv_list.append(root.allocator, shebang.path);

            if (shebang.arg.len > 0) {
                try argv_list.append(root.allocator, shebang.arg);
            }

            try argv_list.appendSlice(root.allocator, argv[1..]);

            file_to_load = try vfs.resolve(null, shebang.path, 0);
            final_argv = argv_list.items;
        }

        var executable = try self.parent.address_space.loadExecutable(file_to_load, 0);
        var entry = executable.entry;

        self.parent.executable = file_to_load;

        if (executable.ld_path) |ld_path| {
            const ld_node = try vfs.resolve(null, ld_path, 0);
            const ld_loaded = try self.parent.address_space.loadExecutable(ld_node, 0x4000_0000);

            entry = ld_loaded.entry;
        }

        var stack = StackHelper.init(self.regs.rsp);
        var old_vm = self.parent.address_space.switchTo();
        var argv_pointers = std.ArrayList(u64).init(root.allocator);
        var envp_pointers = std.ArrayList(u64).init(root.allocator);

        defer argv_pointers.deinit();
        defer envp_pointers.deinit();

        for (final_argv) |string| {
            stack.writeString(string);
            try argv_pointers.append(stack.rsp);
        }

        for (envp) |string| {
            stack.writeString(string);
            try envp_pointers.append(stack.rsp);
        }

        // Align the stack before writing the rest
        stack.rsp &= ~@intCast(u64, 0xF);

        // Write the auxilary vector
        const auxv = [_][2]u64{
            .{ 0, 0 },
            .{ std.elf.AT_ENTRY, executable.aux_vals.at_entry },
            .{ std.elf.AT_PHDR, executable.aux_vals.at_phdr },
            .{ std.elf.AT_PHENT, executable.aux_vals.at_phent },
            .{ std.elf.AT_PHNUM, executable.aux_vals.at_phnum },
        };

        for (auxv) |pair| {
            stack.writeInt(u64, pair[1]);
            stack.writeInt(u64, pair[0]);
        }

        std.mem.reverse(u64, envp_pointers.items);
        std.mem.reverse(u64, argv_pointers.items);

        // Write the environemnt variable pointers
        stack.writeInt(u64, 0);

        for (envp_pointers.items) |pointer| {
            stack.writeInt(u64, pointer);
        }

        // Write the argument pointers
        stack.writeInt(u64, 0);

        for (argv_pointers.items) |pointer| {
            stack.writeInt(u64, pointer);
        }

        // Write the argument count
        stack.writeInt(u64, final_argv.len);

        // Set up the registers
        self.regs.rsp = stack.rsp;
        self.regs.rip = entry;

        _ = old_vm.switchTo();
    }

    pub fn switchTo(self: *Thread, frame: *interrupts.InterruptFrame) void {
        frame.* = self.regs;

        _ = self.parent.address_space.switchTo();
    }
};

pub const Semaphore = struct {
    const Waiter = struct {
        count: usize = undefined,
        thread: *Thread = undefined,
        node: std.TailQueue(void).Node = undefined,
    };

    queue: std.TailQueue(void) = .{},
    lock: IrqSpinlock = .{},
    available: usize,

    pub fn init(count: usize) Semaphore {
        return .{ .available = count };
    }

    pub fn acquire(self: *Semaphore, count: usize) void {
        const ints_enabled = self.lock.lock();
        const thread = per_cpu.get().thread.?;

        if (self.available >= count) {
            self.available -= count;
            self.lock.unlock();

            if (ints_enabled) {
                asm volatile ("sti");
            }
        } else {
            var waiter = Waiter{
                .count = count,
                .thread = thread,
            };

            self.queue.append(&waiter.node);

            ungrabAndReschedule(&self.lock);

            if (ints_enabled) {
                asm volatile ("sti");
            }
        }
    }

    pub fn release(self: *Semaphore, count: usize) void {
        _ = self.lock.lock();

        self.available += count;

        if (self.queue.first) |node| {
            const waiter = @fieldParentPtr(Waiter, "node", node);
            const resources_needed = waiter.count;

            if (self.available >= resources_needed) {
                self.available -= resources_needed;
                self.queue.remove(node);

                enqueue(waiter.thread);
            }
        }

        self.lock.unlock();
    }
};

var scheduler_queue: std.TailQueue(void) = .{};
var scheduler_lock: IrqSpinlock = .{};

var pid_counter: u64 = 1;
var tid_counter: u64 = 0;

var is_idle = false;
var last_is_idle = false;

var kernel_thread: Thread = undefined;
var idle_thread: Thread = undefined;

var kernel_process: process.Process = .{
    .pid = 0,
    .parent = 0,
    .address_space = undefined,
    .exit_code = null,
};

pub fn init() !void {
    interrupts.registerHandler(interrupts.syscall_vector, process.syscallHandler);
    interrupts.registerHandler(interrupts.sched_call_vector, schedCallHandler);

    const root_dir = try vfs.resolve(null, "/", 0);
    const tty = try vfs.resolve(null, "/dev/tty", 0);

    try kernel_process.files.insertAt(0, tty);
    try kernel_process.files.insertAt(1, tty);
    try kernel_process.files.insertAt(2, tty);

    kernel_process.address_space = virt.kernel_address_space.?;
    kernel_process.cwd = root_dir;

    inline for (.{ &kernel_thread, &idle_thread }) |thread| {
        thread.* = .{
            .tid = @atomicRmw(u64, &tid_counter, .Add, 1, .AcqRel),
            .parent = &kernel_process,
        };

        const stack = phys.allocate(1, true) orelse return error.OutOfMemory;

        thread.regs.rsp = virt.hhdm + stack + std.mem.page_size;
        thread.regs.rflags = 0x202;
        thread.regs.cs = 0x28;
        thread.regs.ss = 0x30;
        thread.regs.ds = 0x30;
        thread.regs.es = 0x30;
    }

    idle_thread.regs.rip = @ptrToInt(idleThread);
    kernel_thread.regs.rip = @ptrToInt(root.mainThread);

    enqueue(&kernel_thread);
}

pub fn spawnThread(parent: *process.Process) !*Thread {
    var thread = try root.allocator.create(Thread);

    errdefer root.allocator.destroy(thread);

    thread.* = .{
        .tid = @atomicRmw(u64, &tid_counter, .Add, 1, .AcqRel),
        .parent = parent,
    };

    const stack_pages = 4;
    const stack_base = 0x7FFFFFFF0000;
    const stack_page = phys.allocate(stack_pages, true) orelse return error.OutOfMemory;

    try parent.address_space.page_table.map(
        stack_base,
        stack_page,
        stack_pages * std.mem.page_size,
        virt.Flags.Present | virt.Flags.Writable | virt.Flags.User | virt.Flags.NoExecute,
    );

    thread.regs.rsp = stack_base + stack_pages * std.mem.page_size;
    thread.regs.rflags = 0x202;
    thread.regs.cs = 0x38 | 3;
    thread.regs.ss = 0x40 | 3;
    thread.regs.ds = 0x40 | 3;
    thread.regs.es = 0x40 | 3;

    return thread;
}

pub fn spawnProcess(parent: ?*process.Process) !*process.Process {
    var new_process = try root.allocator.create(process.Process);

    errdefer root.allocator.destroy(new_process);

    new_process.* = .{
        .pid = @atomicRmw(u64, &pid_counter, .Add, 1, .AcqRel),
        .parent = 0,
        .address_space = try virt.createAddressSpace(),
        .exit_code = null,
    };

    if (parent) |parent_proc| {
        new_process.parent = parent_proc.pid;
        new_process.cwd = parent_proc.cwd;
    } else {
        new_process.cwd = try vfs.resolve(null, "/", 0);
    }

    const tty = try vfs.resolve(null, "/dev/tty", 0);

    try new_process.files.insertAt(0, tty);
    try new_process.files.insertAt(1, tty);
    try new_process.files.insertAt(2, tty);

    return new_process;
}

pub fn startKernelThread(entry: fn () noreturn) !*Thread {
    const thread = try spawnThread(&kernel_process);
    const stack = phys.allocate(1, true) orelse return error.OutOfMemory;

    thread.regs.rip = @ptrToInt(entry);
    thread.regs.rsp = virt.hhdm + stack + std.mem.page_size;
    thread.regs.rflags = 0x202;
    thread.regs.cs = 0x28;
    thread.regs.ss = 0x30;
    thread.regs.ds = 0x30;
    thread.regs.es = 0x30;

    enqueue(thread);

    return thread;
}

pub fn enqueue(thread: *Thread) void {
    _ = scheduler_lock.lock();

    defer scheduler_lock.unlock();

    scheduler_queue.append(&thread.node);
}

pub fn dequeueOrNull() ?*Thread {
    _ = scheduler_lock.lock();

    defer scheduler_lock.unlock();

    if (scheduler_queue.pop()) |thread| {
        return @fieldParentPtr(Thread, "node", thread);
    }

    return null;
}

pub fn reschedule(frame: *interrupts.InterruptFrame) void {
    const cpu_info = per_cpu.get();

    if (dequeueOrNull()) |new_thread| {
        if (cpu_info.thread) |old_thread| {
            old_thread.regs = frame.*;
            enqueue(old_thread);
        }

        cpu_info.thread = new_thread;
        new_thread.switchTo(frame);
        is_idle = false;
    } else if (cpu_info.thread == null) {
        idle_thread.switchTo(frame);
        is_idle = true;
    }
}

pub fn ungrabAndReschedule(lock: *IrqSpinlock) void {
    const callback = struct {
        fn func(frame: *interrupts.InterruptFrame, arg: usize) void {
            const spinlock = @intToPtr(*IrqSpinlock, arg);
            const cpu_info = per_cpu.get();
            const thread = cpu_info.thread.?;

            thread.regs = frame.*;
            spinlock.ungrab();
            cpu_info.thread = null;

            reschedule(frame);
        }
    }.func;

    schedCall(callback, @ptrToInt(lock));
}

pub fn schedCall(func: fn (*interrupts.InterruptFrame, usize) void, arg: usize) void {
    asm volatile ("int %[vec]"
        :
        : [vec] "i" (interrupts.sched_call_vector),
          [_] "{rax}" (@ptrToInt(func)),
          [_] "{rcx}" (arg),
    );
}

fn schedCallHandler(frame: *interrupts.InterruptFrame) void {
    const func = @intToPtr(fn (*interrupts.InterruptFrame, usize) void, frame.rax);

    func(frame, frame.rcx);
}

fn idleThread() noreturn {
    while (true) {
        if (is_idle != last_is_idle) {
            logger.debug("System is now idle", .{});
        }

        last_is_idle = is_idle;

        arch.halt();
    }
}
