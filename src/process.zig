const logger = std.log.scoped(.process);

const root = @import("root");
const std = @import("std");

const vfs = @import("vfs.zig");
const interrupts = @import("interrupts.zig");
const scheduler = @import("scheduler.zig");
const per_cpu = @import("per_cpu.zig");
const virt = @import("virt.zig");

pub const SyscallNumber = enum(u64) {
    ProcExit = 0x0,
    ProcLog = 0x1,

    FileOpen = 0x100,
    FileClose = 0x101,
    FileRead = 0x102,
    FileWrite = 0x103,
    FileSeek = 0x104,

    MemMap = 0x200,
    MemUnmap = 0x201,
    MemProtect = 0x202,

    _,
};

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

    pub fn remove(self: *FileTable, fd: u64) bool {
        return self.files.swapRemove(fd);
    }
};

fn sliceToMany(comptime T: type) type {
    comptime var typ = @typeInfo(T);

    typ.Pointer.size = .Many;

    return @Type(typ);
}

pub const Process = struct {
    pid: u64,
    parent: u64,
    cwd: *vfs.VNode = undefined,
    executable: *vfs.VNode = undefined,
    children: std.ArrayListUnmanaged(*Process) = .{},
    threads: std.ArrayListUnmanaged(*scheduler.Thread) = .{},
    files: FileTable = .{},
    address_space: virt.AddressSpace,
    exit_code: ?u8,

    fn validateString(self: *Process, comptime T: type, ptr: u64) !T {
        _ = self;

        return std.mem.span(@intToPtr(sliceToMany(T), ptr));
    }

    fn validateBuffer(self: *Process, comptime T: type, ptr: u64, len: u64) !T {
        _ = self;

        return @intToPtr(sliceToMany(T), ptr)[0..len];
    }
};

pub fn syscallHandler(frame: *interrupts.InterruptFrame) void {
    frame.rax = syscallHandlerImpl(frame) catch |err| blk: {
        const errno = @as(std.os.linux.E, switch (err) {
            error.AccessDenied => .ACCES,
            error.SymLinkLoop => .LOOP,
            error.ProcessFdQuotaExceeded => .DQUOT,
            error.SystemFdQuotaExceeded => .DQUOT,
            error.NoDevice => .NXIO,
            error.FileNotFound => .NOENT,
            error.NameTooLong => .NAMETOOLONG,
            error.SystemResources => .NOMEM,
            error.FileTooBig => .FBIG,
            error.IsDir => .ISDIR,
            error.NoSpaceLeft => .NOSPC,
            error.NotDir => .NOTDIR,
            error.PathAlreadyExists => .EXIST,
            error.DeviceBusy => .BUSY,
            error.FileLocksNotSupported => .NOLCK,
            error.BadPathName => .FAULT,
            error.InvalidUtf8 => .INVAL,
            error.FileBusy => .BUSY,
            error.WouldBlock => .AGAIN,
            error.Unexpected => unreachable,
            error.OutOfMemory => .NOMEM,
            error.NotImplemented => .NOSYS,
            error.NotFound => .NOENT,
            error.BadFileDescriptor => .BADFD,
            error.InputOutput => .IO,
            error.OperationAborted => .CANCELED,
            error.BrokenPipe => .PIPE,
            error.ConnectionResetByPeer => .CONNRESET,
            error.ConnectionTimedOut => .TIMEDOUT,
            error.NotOpenForReading => .BADF,
            error.Unseekable => .SPIPE,
            error.DiskQuota => .DQUOT,
            error.NotOpenForWriting => .ROFS,
            error.InvalidArgument => .INVAL,
        });

        break :blk @bitCast(u64, @as(i64, -@bitCast(i16, @enumToInt(errno))));
    } orelse return;
}

fn syscallHandlerImpl(frame: *interrupts.InterruptFrame) !?u64 {
    const cpu_info = per_cpu.get();
    const syscall_num = @intToEnum(SyscallNumber, frame.rax);
    const process = cpu_info.currentProcess().?;

    return switch (syscall_num) {
        .ProcExit => {
            cpu_info.thread = null;

            process.exit_code = @truncate(u8, frame.rdi);

            logger.debug("Exiting process {} with code {}", .{ process.pid, process.exit_code });

            scheduler.reschedule(frame);

            return null;
        },
        .ProcLog => {
            const buffer = try process.validateString([:0]const u8, frame.rdi);
            const length = std.mem.len(buffer);

            logger.info("{s}", .{buffer[0..length]});

            return 0;
        },
        .FileOpen => {
            const path = try process.validateString([:0]const u8, frame.rdi);
            const vnode = if (std.fs.path.isAbsolute(path))
                try vfs.resolve(null, path, frame.rsi)
            else
                try vfs.resolve(process.cwd, path, frame.rsi);

            return try process.files.insert(vnode);
        },
        .FileClose => {
            if (process.files.remove(frame.rdi)) {
                return 0;
            } else {
                return error.BadFileDescriptor;
            }
        },
        .FileRead => {
            const file = process.files.get(frame.rdi) orelse return error.BadFileDescriptor;
            const buffer = try process.validateBuffer([]u8, frame.rsi, frame.rdx);
            const bytes_read = try file.vnode.read(buffer, file.offset);

            file.offset += bytes_read;

            return bytes_read;
        },
        .FileWrite => {
            const file = process.files.get(frame.rdi) orelse return error.BadFileDescriptor;
            const buffer = try process.validateBuffer([]const u8, frame.rsi, frame.rdx);
            const bytes_written = try file.vnode.write(buffer, file.offset);

            file.offset += bytes_written;

            return bytes_written;
        },
        .FileSeek => {
            const file = process.files.get(frame.rdi) orelse return error.BadFileDescriptor;

            switch (frame.rdx) {
                std.os.linux.SEEK.SET => file.offset = frame.rsi,
                std.os.linux.SEEK.CUR => file.offset +%= frame.rsi,
                std.os.linux.SEEK.END => unreachable,
                else => return error.InvalidArgument,
            }

            return file.offset;
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
