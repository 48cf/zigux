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

pub const Thread = struct {
    tid: u64,
    parent: *process.Process,
    regs: interrupts.InterruptFrame = blk: {
        var regs = std.mem.zeroes(interrupts.InterruptFrame);

        regs.rflags = 0x202;

        break :blk regs;
    },
    node: std.TailQueue(void).Node = undefined,

    pub fn exec(self: *Thread, file: *vfs.VNode) !void {
        var stream = file.stream();
        var header = try std.elf.Header.read(&stream);
        var ph_iter = header.program_header_iterator(&stream);

        while (try ph_iter.next()) |ph| {
            if (ph.p_type != 1) {
                continue;
            }

            const alloc_size = std.math.max(ph.p_filesz, ph.p_memsz);

            var offset: usize = 0;
            var flags: u64 = virt.Flags.Present | virt.Flags.User;

            if (ph.p_flags & std.elf.PF_W != 0)
                flags |= virt.Flags.Writable;

            if (ph.p_flags & std.elf.PF_X == 0)
                flags |= virt.Flags.NoExecute;

            while (offset < alloc_size) : (offset += std.mem.page_size) {
                const page_phys = phys.allocate(1, true) orelse return error.OutOfMemory;
                const page_hh = @intToPtr([*]u8, virt.hhdm + page_phys)[0..std.mem.page_size];

                try self.parent.address_space.page_table.mapPage(ph.p_vaddr + offset, page_phys, flags);

                std.mem.set(u8, page_hh, 0);

                if (offset < ph.p_filesz) {
                    const copy_size = std.math.min(ph.p_filesz - offset, std.mem.page_size);
                    const file_offset = ph.p_offset + offset;

                    _ = try file.read(page_hh[0..copy_size], file_offset);

                    // logger.debug("Copying {} bytes at offset 0x{X} from file at file offset 0x{X}", .{ copy_size, offset, file_offset });
                }
            }
        }

        self.regs.rip = header.entry;
    }

    pub fn switchTo(self: *Thread, frame: *interrupts.InterruptFrame) void {
        frame.* = self.regs;

        asm volatile ("mov %[cr3], %%cr3"
            :
            : [cr3] "r" (self.parent.address_space.cr3),
        );
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

    const stack_base = 0x7FFFFFFF0000;
    const stack_page = phys.allocate(1, true) orelse return error.OutOfMemory;

    try parent.address_space.page_table.mapPage(
        stack_base,
        stack_page,
        virt.Flags.Present | virt.Flags.Writable | virt.Flags.User | virt.Flags.NoExecute,
    );

    thread.regs.rsp = stack_base + std.mem.page_size;
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

pub fn dequeue() *Thread {
    while (true) {
        return dequeueOrNull() orelse continue;
    }
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
    } else if (cpu_info.thread == null) {
        idle_thread.switchTo(frame);
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
        arch.halt();
    }
}
