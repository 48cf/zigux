const logger = std.log.scoped(.process);

const root = @import("root");
const std = @import("std");

const vfs = @import("vfs.zig");
const interrupts = @import("interrupts.zig");
const scheduler = @import("scheduler.zig");
const per_cpu = @import("per_cpu.zig");
const virt = @import("virt.zig");

const FileDescriptor = struct {
    vnode: *vfs.VNode,
    offset: usize,
};

const FileTable = struct {
    files: std.AutoArrayHashMapUnmanaged(u64, FileDescriptor) = .{},
    fd_counter: u64 = 0,

    pub fn insertAt(self: *FileTable, fd: u64, vnode: *vfs.VNode) !void {
        self.fd_counter = std.math.max(fd + 1, self.fd_counter);

        try self.files.put(root.allocator, fd, .{
            .vnode = vnode,
            .offset = 0,
        });
    }

    pub fn insert(self: *FileTable, vnode: *vfs.VNode) !u64 {
        const fd = self.fd_counter;

        try self.insertAt(fd, vnode);

        return fd;
    }

    pub fn get(self: *FileTable, fd: u64) ?*FileDescriptor {
        return self.files.getPtr(fd);
    }
};

pub const Process = struct {
    pid: u64,
    parent: u64,
    cwd: *vfs.VNode = undefined,
    children: std.ArrayListUnmanaged(*Process) = .{},
    threads: std.ArrayListUnmanaged(*scheduler.Thread) = .{},
    files: FileTable = .{},
    address_space: virt.AddressSpace,
    exit_code: ?u8,
};

pub fn syscallHandler(frame: *interrupts.InterruptFrame) void {
    frame.rax = syscallHandlerImpl(frame) catch |err| {
        const linux_error = @as(std.os.linux.E, switch (err) {
            error.NotImplemented => .NOSYS,
            // error.InvalidArgument => .INVAL,
            // error.BadFileDescriptor => .BADF,
            // error.OutOfMemory => .NOMEM,
        });

        frame.rax = @bitCast(u64, @as(i64, -@bitCast(i16, @enumToInt(linux_error))));

        return;
    } orelse return;
}

fn syscallHandlerImpl(frame: *interrupts.InterruptFrame) !?u64 {
    const cpu_info = per_cpu.get();
    const syscall_num = @intToEnum(std.os.linux.SYS, frame.rax);

    return switch (syscall_num) {
        .exit => {
            if (cpu_info.thread) |thread| {
                thread.parent.exit_code = @truncate(u8, frame.rdi);
                cpu_info.thread = null;

                logger.debug(
                    "Exiting process {} with code {}",
                    .{ thread.parent.pid, thread.parent.exit_code },
                );
            }

            scheduler.reschedule(frame);

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
