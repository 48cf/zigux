const logger = std.log.scoped(.scheduler);

const root = @import("root");
const std = @import("std");

const arch = @import("arch.zig");
const interrupts = @import("interrupts.zig");
const phys = @import("phys.zig");
const per_cpu = @import("per_cpu.zig");
const virt = @import("virt.zig");
const utils = @import("utils.zig");
const IrqSpinlock = @import("irq_lock.zig").IrqSpinlock;

pub const Thread = struct {
    tid: u64,
    parent: *Process,
    regs: interrupts.InterruptFrame = blk: {
        var regs = std.mem.zeroes(interrupts.InterruptFrame);

        regs.rflags = 0x202;

        break :blk regs;
    },
    node: std.TailQueue(void).Node = undefined,

    pub fn exec(self: *Thread, file: []align(8) const u8) !void {
        _ = self;

        const header = try std.elf.Header.parse(file[0..64]);

        var buffer = std.io.fixedBufferStream(file);
        var ph_iter = header.program_header_iterator(buffer);

        logger.info("{}", .{header});

        while (try ph_iter.next()) |ph| {
            logger.info("{}", .{ph});

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
                    const source = file[file_offset .. file_offset + copy_size];

                    std.mem.copy(u8, page_hh, source);

                    logger.debug("Copying {} bytes at offset 0x{X} from file at file offset 0x{X}", .{ copy_size, offset, file_offset });
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

pub const Process = struct {
    pid: u64,
    parent: u64,
    children: std.ArrayListUnmanaged(*Process) = .{},
    threads: std.ArrayListUnmanaged(*Thread) = .{},
    address_space: virt.AddressSpace,
    exit_code: ?u8,
};

var scheduler_queue: std.TailQueue(void) = .{};
var scheduler_lock: IrqSpinlock = .{};

var pid_counter: u64 = 1;
var tid_counter: u64 = 0;

var kernel_thread: Thread = undefined;
var idle_thread: Thread = undefined;

var kernel_process: Process = .{
    .pid = 0,
    .parent = 0,
    .address_space = undefined,
    .exit_code = null,
};

pub fn init() !void {
    interrupts.register_handler(interrupts.syscall_vector, syscallHandler);

    kernel_process.address_space = virt.kernel_address_space.?;

    inline for (.{ &idle_thread, &kernel_thread }) |thread| {
        thread.* = .{
            .tid = @atomicRmw(u64, &tid_counter, .Add, 1, .AcqRel),
            .parent = &kernel_process,
        };

        thread.regs.rflags = 0x202;
        thread.regs.cs = 0x28;
        thread.regs.ss = 0x30;
        thread.regs.ds = 0x30;
        thread.regs.es = 0x30;
    }

    idle_thread.regs.rip = @ptrToInt(idleThread);

    const kernel_stack = phys.allocate(1, true) orelse return error.OutOfMemory;

    kernel_thread.regs.rip = @ptrToInt(root.mainThread);
    kernel_thread.regs.rsp = virt.hhdm + kernel_stack + std.mem.page_size;

    enqueue(&kernel_thread);
}

pub fn spawnThread(parent: *Process) !*Thread {
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

pub fn spawnProcess(parent: ?u64) !*Process {
    var process = try root.allocator.create(Process);

    errdefer root.allocator.destroy(process);

    process.* = .{
        .pid = @atomicRmw(u64, &pid_counter, .Add, 1, .AcqRel),
        .parent = parent orelse 0,
        .address_space = try virt.createAddressSpace(),
        .exit_code = null,
    };

    return process;
}

pub fn enqueue(thread: *Thread) void {
    scheduler_lock.lock();

    defer scheduler_lock.unlock();

    scheduler_queue.append(&thread.node);
}

pub fn dequeue() *Thread {
    while (true) {
        return dequeueOrNull() orelse continue;
    }
}

pub fn dequeueOrNull() ?*Thread {
    scheduler_lock.lock();

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

fn syscallHandler(frame: *interrupts.InterruptFrame) void {
    frame.rax = syscallHandlerImpl(frame) catch |err| {
        const linux_error = @as(std.os.linux.E, switch (err) {
            error.NotImplemented => .NOSYS,
        });

        frame.rax = @bitCast(u64, @as(i64, -@bitCast(i16, @enumToInt(linux_error))));

        return;
    } orelse return;
}

fn syscallHandlerImpl(frame: *interrupts.InterruptFrame) !?u64 {
    const cpu_info = per_cpu.get();
    const syscall_num = @intToEnum(std.os.linux.SYS, frame.rax);

    return switch (syscall_num) {
        .open => {
            const path = std.mem.span(@intToPtr([*:0]const u8, frame.rdi));

            logger.debug("Attempt to open file {s}", .{path});

            return error.NotImplemented;
        },
        .exit => {
            cpu_info.thread.?.parent.exit_code = @truncate(u8, frame.rdi);
            cpu_info.thread = null;

            reschedule(frame);

            return null;
        },
        else => {
            logger.warn(
                "Unimplemented syscall {}: {{ 0x{X}, 0x{X}, 0x{X}, 0x{X}, 0x{X}, 0x{X} }}",
                .{ syscall_num, frame.rdi, frame.rsi, frame.rdx, frame.r10, frame.r8, frame.r9 },
            );

            return error.NotImplemented;
        },
    };
}

fn idleThread() callconv(.Naked) noreturn {
    while (true) {
        arch.halt();
    }
}
