const logger = std.log.scoped(.scheduler);

const root = @import("root");
const std = @import("std");

const abi = @import("abi.zig");
const arch = @import("arch.zig");
const interrupts = @import("interrupts.zig");
const process = @import("process.zig");
const phys = @import("phys.zig");
const per_cpu = @import("per_cpu.zig");
const virt = @import("virt.zig");
const utils = @import("utils.zig");
const vfs = @import("vfs.zig");

const IrqSpinlock = @import("irq_lock.zig").IrqSpinlock;

pub const syscall_stack_pages = 4;
pub const thread_stack_pages = 8;

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

    _ = try file.read(@ptrCast([*]u8, &char)[0..1], offset, 0);

    if (char == ' ') {
        offset += 1;
    }

    while (true) {
        _ = try file.read(@ptrCast([*]u8, &char)[0..1], offset, 0);

        offset += 1;

        switch (char) {
            ' ' => break,
            '\n' => return Shebang{ .path = path.items, .arg = "" },
            else => try path.append(root.allocator, char),
        }
    }

    while (true) {
        _ = try file.read(@ptrCast([*]u8, &char)[0..1], offset, 0);

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
    syscall_stack: u64 = 0,

    pub fn exec(
        self: *Thread,
        file: *vfs.VNode,
        argv: []const []const u8,
        envp: []const []const u8,
    ) !void {
        var shebang_sig = [2]u8{ 0, 0 };

        _ = try file.read(&shebang_sig, 0, 0);

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

        var old_vm = self.parent.address_space.switchTo();
        var executable = try self.parent.address_space.loadExecutable(file_to_load, 0);
        var entry = executable.entry;

        self.parent.executable = file_to_load;

        if (executable.ld_path) |ld_path| {
            logger.debug("Dynamic linker for {s} is at {s}", .{ file_to_load.getFullPath(), ld_path });

            const ld_node = try vfs.resolve(null, ld_path, 0);
            const ld_loaded = try self.parent.address_space.loadExecutable(ld_node, 0x4000_0000);

            entry = ld_loaded.entry;
        }

        var stack = StackHelper.init(self.regs.rsp);
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

        const auxv = [_][2]u64{
            .{ std.elf.AT_ENTRY, executable.aux_vals.at_entry },
            .{ std.elf.AT_PHDR, executable.aux_vals.at_phdr },
            .{ std.elf.AT_PHENT, executable.aux_vals.at_phent },
            .{ std.elf.AT_PHNUM, executable.aux_vals.at_phnum },
        };

        stack.rsp = utils.alignDown(u64, stack.rsp, 16);

        // Calculate the amount of stuff we will write to the stack and misalign
        // it on purpose so it's 16 byte aligned at the process entry after all the pushes
        const pointer_count = auxv.len * 2 + envp_pointers.items.len + argv_pointers.items.len + 4;
        const byte_count = pointer_count * 8;

        if (!utils.isAligned(u64, stack.rsp - byte_count, 16)) {
            stack.rsp -= 8;
        }

        // Write the auxilary vector
        stack.writeInt(u64, 0);

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

        std.debug.assert(utils.isAligned(u64, stack.rsp, 16));

        // Set up the registers
        self.regs.rsp = stack.rsp;
        self.regs.rip = entry;

        _ = old_vm.switchTo();
    }

    pub fn switchTo(self: *Thread, frame: *interrupts.InterruptFrame) void {
        const cpu_info = per_cpu.get();

        cpu_info.tss.rsp[0] = self.syscall_stack + syscall_stack_pages * std.mem.page_size;

        frame.* = self.regs;

        _ = self.parent.address_space.switchTo();
    }
};

pub const Semaphore = struct {
    const Waiter = struct {
        count: isize = undefined,
        thread: *Thread = undefined,
        node: std.TailQueue(void).Node = undefined,
    };

    queue: std.TailQueue(void) = .{},
    lock: IrqSpinlock = .{},
    available: isize,

    pub fn init(count: isize) Semaphore {
        return .{ .available = count };
    }

    pub fn acquire(self: *Semaphore, count: isize) void {
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

    pub fn release(self: *Semaphore, count: isize) void {
        _ = self.lock.lock();

        self.available += count;

        while (self.queue.first) |node| {
            const waiter = @fieldParentPtr(Waiter, "node", node);
            const resources_needed = waiter.count;

            if (self.available >= resources_needed) {
                self.available -= resources_needed;
                self.queue.remove(node);

                enqueue(waiter.thread);
            } else {
                break;
            }
        }

        self.lock.unlock();
    }
};

var processes: std.TailQueue(void) = .{};
var scheduler_queue: std.TailQueue(void) = .{};
var scheduler_lock: IrqSpinlock = .{};

var pid_counter: u64 = 1;
var tid_counter: u64 = 0;

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

    kernel_process.address_space = virt.kernel_address_space;
    kernel_process.cwd = root_dir;

    const stack = phys.allocate(1, true) orelse return error.OutOfMemory;

    idle_thread = .{
        .tid = @atomicRmw(u64, &tid_counter, .Add, 1, .AcqRel),
        .parent = &kernel_process,
    };

    idle_thread.regs.rsp = virt.asHigherHalf(u64, stack + std.mem.page_size);
    idle_thread.regs.rip = @ptrToInt(idleThread);
    idle_thread.regs.rflags = 0x202;
    idle_thread.regs.cs = 0x28;
    idle_thread.regs.ss = 0x30;
    idle_thread.regs.ds = 0x30;
    idle_thread.regs.es = 0x30;
}

pub fn spawnThread(parent: *process.Process) !*Thread {
    const thread = try spawnThreadWithoutStack(parent);

    errdefer root.allocator.destroy(thread);

    const stack_base = try parent.address_space.mmap(
        0,
        thread_stack_pages * std.mem.page_size,
        abi.PROT_READ | abi.PROT_WRITE,
        abi.MAP_PRIVATE | abi.MAP_FIXED,
        null,
        0,
    );

    thread.regs.rsp = stack_base + thread_stack_pages * std.mem.page_size;

    return thread;
}

pub fn spawnThreadWithoutStack(parent: *process.Process) !*Thread {
    var thread = try root.allocator.create(Thread);

    errdefer root.allocator.destroy(thread);

    thread.* = .{
        .tid = @atomicRmw(u64, &tid_counter, .Add, 1, .AcqRel),
        .parent = parent,
    };

    const syscall_stack = phys.allocate(syscall_stack_pages, false) orelse return error.OutOfMemory;

    thread.syscall_stack = virt.asHigherHalf(u64, syscall_stack);
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

    processes.append(&new_process.scheduler_node);

    return new_process;
}

pub fn getProcessByPid(pid: u64) ?*process.Process {
    var iter = processes.first;

    while (iter) |node| : (iter = node.next) {
        const proc = @fieldParentPtr(process.Process, "scheduler_node", node);

        if (proc.pid == pid) {
            return proc;
        }
    }

    return null;
}

pub fn startKernelThread(comptime entry: anytype, context: anytype) !*Thread {
    const thread = try root.allocator.create(Thread);
    const stack = phys.allocate(4, true) orelse return error.OutOfMemory;

    errdefer root.allocator.destroy(thread);

    thread.* = .{
        .tid = @atomicRmw(u64, &tid_counter, .Add, 1, .AcqRel),
        .parent = &kernel_process,
    };

    const wrapper = struct {
        fn handler(arg: u64) callconv(.C) noreturn {
            const entry_type = @typeInfo(@TypeOf(entry)).Fn;
            const result = switch (@typeInfo(entry_type.args[0].arg_type.?)) {
                .Pointer => @call(.{ .modifier = .always_inline }, entry, .{@intToPtr(entry_type.args[0].arg_type.?, arg)}),
                else => @call(.{ .modifier = .always_inline }, entry, .{@as(entry_type.args[0].arg_type.?, arg)}),
            };

            switch (@typeInfo(@TypeOf(result))) {
                .ErrorUnion => result catch |err| {
                    std.debug.panicExtra(@errorReturnTrace(), "Unhandled error occurred: {err}", .{err});
                },
                else => {},
            }

            exitThread();
        }
    };

    thread.regs.rip = @ptrToInt(wrapper.handler);
    thread.regs.rsp = virt.asHigherHalf(u64, stack + 4 * std.mem.page_size - 0x10);
    thread.regs.rdi = switch (@typeInfo(@TypeOf(context))) {
        .Pointer => @ptrToInt(context),
        else => @as(u64, context),
    };
    thread.regs.rflags = 0x202;
    thread.regs.cs = 0x28;
    thread.regs.ss = 0x30;
    thread.regs.ds = 0x30;
    thread.regs.es = 0x30;

    enqueue(thread);

    return thread;
}

pub fn forkProcess(parent: *process.Process) !*process.Process {
    const new_process = try root.allocator.create(process.Process);

    errdefer root.allocator.destroy(new_process);

    new_process.* = .{
        .pid = @atomicRmw(u64, &pid_counter, .Add, 1, .AcqRel),
        .parent = parent.pid,
        .cwd = parent.cwd,
        .files = try parent.files.fork(),
        .address_space = try parent.address_space.fork(),
        .exit_code = null,
    };

    processes.append(&new_process.scheduler_node);

    return new_process;
}

pub fn exitProcess(proc: *process.Process, exit_code: u8) void {
    proc.exit_code = exit_code;
    proc.waitpid_semaphore.release(1);

    if (getProcessByPid(proc.parent)) |parent| {
        processes.remove(&proc.scheduler_node);
        parent.zombie_children.append(&proc.scheduler_node);
        parent.waitpid_child_semaphore.release(1);
    }

    logger.debug("Exiting process {} with code {}", .{ proc.pid, proc.exit_code });
}

pub fn exitThread() noreturn {
    per_cpu.get().thread = null;

    while (true) {
        yield();
    }
}

pub fn enqueue(thread: *Thread) void {
    _ = scheduler_lock.lock();

    defer scheduler_lock.unlock();

    scheduler_queue.append(&thread.node);
}

pub fn dequeueOrNull() ?*Thread {
    _ = scheduler_lock.lock();

    defer scheduler_lock.unlock();

    if (scheduler_queue.popFirst()) |thread| {
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
    } else if (cpu_info.thread == null) {
        idle_thread.switchTo(frame);
    }
}

pub fn yield() void {
    schedCall(struct {
        fn handler(frame: *interrupts.InterruptFrame, arg: usize) void {
            _ = arg;
            reschedule(frame);
        }
    }.handler, undefined);
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
        arch.halt();
    }
}
