const logger = std.log.scoped(.process);

const root = @import("root");
const std = @import("std");

const abi = @import("abi.zig");
const arch = @import("arch.zig");
const vfs = @import("vfs.zig");
const interrupts = @import("interrupts.zig");
const scheduler = @import("scheduler.zig");
const per_cpu = @import("per_cpu.zig");
const virt = @import("virt.zig");

const all_prot: u64 = abi.PROT_READ | abi.PROT_WRITE | abi.PROT_EXEC;
const all_flags: u64 = abi.MAP_SHARED | abi.MAP_PRIVATE | abi.MAP_FIXED | abi.MAP_ANON;

pub const SyscallNumber = enum(u64) {
    ProcExit = 0x0,
    ProcLog = 0x1,
    ProcArchCtl = 0x2,
    ProcGetPid = 0x3,
    ProcGetPPid = 0x4,
    ProcFork = 0x5,
    ProcExec = 0x6,
    ProcWait = 0x7,

    FileOpen = 0x100,
    FileClose = 0x101,
    FileRead = 0x102,
    FileWrite = 0x103,
    FileSeek = 0x104,
    FileGetCwd = 0x105,
    FileStatFd = 0x106,
    FileStatPath = 0x107,
    FileIoControl = 0x108,
    FileReadDir = 0x109,
    FileChdir = 0x10A,

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

    pub fn fork(self: *FileTable) !FileTable {
        var new_table = FileTable{ .fd_counter = self.fd_counter };

        var iterator = self.files.iterator();

        while (iterator.next()) |entry| {
            try new_table.files.put(root.allocator, entry.key_ptr.*, entry.value_ptr.*);
        }

        return new_table;
    }

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
    scheduler_node: std.TailQueue(void).Node = undefined,
    waitpid_semaphore: scheduler.Semaphore = .{ .available = -1 },
    waitpid_child_semaphore: scheduler.Semaphore = .{ .available = 0 },
    zombie_children: std.TailQueue(void) = .{},
    files: FileTable = .{},
    address_space: virt.AddressSpace,
    exit_code: ?u8,

    pub fn validateSentinel(self: *Process, comptime T: type, comptime sentinel: T, ptr: u64) ![:sentinel]const T {
        _ = self;

        return std.mem.span(@intToPtr([*:sentinel]const T, ptr));
    }

    pub fn validateBuffer(self: *Process, comptime T: type, ptr: u64, len: u64) !T {
        _ = self;

        return @intToPtr(sliceToMany(T), ptr)[0..len];
    }

    pub fn validatePointer(self: *Process, comptime T: type, ptr: u64) !*T {
        _ = self;

        return @intToPtr(*T, ptr);
    }
};

const errorTypeMap = .{
    .{ error.AccessDenied, abi.EACCES },
    .{ error.SymLinkLoop, abi.ELOOP },
    .{ error.ProcessFdQuotaExceeded, abi.EDQUOT },
    .{ error.SystemFdQuotaExceeded, abi.EDQUOT },
    .{ error.NoDevice, abi.ENXIO },
    .{ error.FileNotFound, abi.ENOENT },
    .{ error.NameTooLong, abi.ENAMETOOLONG },
    .{ error.SystemResources, abi.ENOMEM },
    .{ error.FileTooBig, abi.EFBIG },
    .{ error.IsDir, abi.EISDIR },
    .{ error.NoSpaceLeft, abi.ENOSPC },
    .{ error.NotDir, abi.ENOTDIR },
    .{ error.PathAlreadyExists, abi.EEXIST },
    .{ error.DeviceBusy, abi.EBUSY },
    .{ error.FileLocksNotSupported, abi.ENOLCK },
    .{ error.BadPathName, abi.EFAULT },
    .{ error.InvalidUtf8, abi.EINVAL },
    .{ error.FileBusy, abi.EBUSY },
    .{ error.WouldBlock, abi.EAGAIN },
    .{ error.OutOfMemory, abi.ENOMEM },
    .{ error.NotImplemented, abi.ENOSYS },
    .{ error.NotFound, abi.ENOENT },
    .{ error.BadFileDescriptor, abi.EBADFD },
    .{ error.InputOutput, abi.EIO },
    .{ error.OperationAborted, abi.ECANCELED },
    .{ error.BrokenPipe, abi.EPIPE },
    .{ error.ConnectionResetByPeer, abi.ECONNRESET },
    .{ error.ConnectionTimedOut, abi.ETIMEDOUT },
    .{ error.NotOpenForReading, abi.EBADF },
    .{ error.Unseekable, abi.ESPIPE },
    .{ error.DiskQuota, abi.EDQUOT },
    .{ error.NotOpenForWriting, abi.EROFS },
    .{ error.InvalidArgument, abi.EINVAL },
    .{ error.InvalidHandle, abi.EINVAL },
};

fn errnoToError(errno: u16) anyerror {
    inline for (errorTypeMap) |pair| {
        if (pair[1] == errno) {
            return pair[0];
        }
    }

    return error.Unexpected;
}

fn errorToErrno(err: anyerror) u16 {
    inline for (errorTypeMap) |pair| {
        if (pair[0] == err) {
            return pair[1];
        }
    }

    unreachable;
}

pub fn syscallHandler(frame: *interrupts.InterruptFrame) void {
    frame.rax = syscallHandlerImpl(frame) catch |err| blk: {
        const errno = errorToErrno(err);

        break :blk @bitCast(u64, @as(i64, -@bitCast(i16, errno)));
    } orelse return;
}

fn syscallHandlerImpl(frame: *interrupts.InterruptFrame) !?u64 {
    const cpu_info = per_cpu.get();
    const syscall_num = @intToEnum(SyscallNumber, frame.rax);
    const process = cpu_info.currentProcess().?;

    return switch (syscall_num) {
        .ProcExit => {
            cpu_info.thread = null;

            scheduler.exitProcess(process, @truncate(u8, frame.rdi));
            scheduler.reschedule(frame);

            return null;
        },
        .ProcLog => {
            const buffer = try process.validateSentinel(u8, 0, frame.rdi);
            const length = std.mem.len(buffer);

            logger.info("{s}", .{buffer[0..length]});

            return 0;
        },
        .ProcArchCtl => {
            switch (frame.rdi) {
                abi.ARCH_SET_FS => {
                    arch.Msr.fs_base.write(frame.rsi);

                    return 0;
                },
                else => return error.InvalidArgument,
            }
        },
        .ProcGetPid => return process.pid,
        .ProcGetPPid => return process.parent,
        .ProcFork => {
            const forked_process = try scheduler.forkProcess(process);
            const new_thread = try scheduler.spawnThreadWithoutStack(forked_process);

            new_thread.regs = frame.*;
            new_thread.regs.rax = 0;

            scheduler.enqueue(new_thread);

            return forked_process.pid;
        },
        .ProcExec => {
            const thread = cpu_info.thread.?;
            const path = try process.validateSentinel(u8, 0, frame.rdi);
            const file = try vfs.resolve(process.cwd, path, 0);

            const argv = try process.validateSentinel(?[*:0]const u8, null, frame.rsi);
            const envp = try process.validateSentinel(?[*:0]const u8, null, frame.rdx);

            var argv_copy = std.ArrayList([]u8).init(root.allocator);
            var envp_copy = std.ArrayList([]u8).init(root.allocator);

            for (argv) |arg_opt| {
                const arg = try process.validateSentinel(u8, 0, @ptrToInt(arg_opt.?));
                try argv_copy.append(try root.allocator.dupe(u8, arg));
            }

            for (envp) |env_opt| {
                const env = try process.validateSentinel(u8, 0, @ptrToInt(env_opt.?));
                try envp_copy.append(try root.allocator.dupe(u8, env));
            }

            process.address_space = try virt.createAddressSpace();

            _ = process.address_space.switchTo();

            const stack_base = try process.address_space.mmap(
                0,
                scheduler.thread_stack_pages * std.mem.page_size,
                abi.PROT_READ | abi.PROT_WRITE,
                abi.MAP_PRIVATE | abi.MAP_FIXED,
                null,
                0,
            );

            thread.regs = std.mem.zeroes(interrupts.InterruptFrame);
            thread.regs.rsp = stack_base + scheduler.thread_stack_pages * std.mem.page_size;
            thread.regs.rflags = 0x202;
            thread.regs.cs = 0x38 | 3;
            thread.regs.ss = 0x40 | 3;
            thread.regs.ds = 0x40 | 3;
            thread.regs.es = 0x40 | 3;

            try thread.exec(file, argv_copy.items, envp_copy.items);

            frame.* = thread.regs;

            return null;
        },
        .ProcWait => {
            const status = try process.validatePointer(c_int, frame.rsi);
            const pid = @truncate(c_int, @bitCast(i64, frame.rdi));

            if (pid > 0) {
                const target_proc = scheduler.getProcessByPid(@intCast(u64, pid)) orelse return error.InvalidArgument;

                target_proc.waitpid_semaphore.acquire(0);
                status.* = target_proc.exit_code.?;

                return target_proc.pid;
            } else if (pid == -1) {
                process.waitpid_child_semaphore.acquire(1);

                const node = process.zombie_children.pop().?;
                const child = @fieldParentPtr(Process, "scheduler_node", node);

                status.* = child.exit_code.?;

                return child.pid;
            } else {
                logger.warn("Unknown wait target {}", .{pid});

                return error.InvalidArgument;
            }
        },
        .FileOpen => {
            const path = try process.validateSentinel(u8, 0, frame.rdi);
            const vnode = try vfs.resolve(process.cwd, path, frame.rsi);

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
                abi.SEEK_SET => file.offset = frame.rsi,
                abi.SEEK_CUR => file.offset +%= frame.rsi,
                abi.SEEK_END => unreachable,
                else => return error.InvalidArgument,
            }

            return file.offset;
        },
        .FileGetCwd => {
            var string_buf = try process.validateBuffer([]u8, frame.rdi, frame.rsi);
            var buffer = std.io.fixedBufferStream(string_buf);
            var writer = buffer.writer();

            try writer.print("{}", .{process.cwd.getFullPath()});
            try writer.writeByte(0);

            return std.mem.len(@ptrCast([*:0]const u8, string_buf));
        },
        .FileStatFd, .FileStatPath => {
            const buffer = try process.validatePointer(abi.stat, frame.rsi);
            const file = if (syscall_num == .FileStatFd) blk: {
                const fd = process.files.get(frame.rdi) orelse return error.BadFileDescriptor;
                break :blk fd.vnode;
            } else if (syscall_num == .FileStatPath) blk: {
                const path = try process.validateSentinel(u8, 0, frame.rdi);
                break :blk try vfs.resolve(process.cwd, path, 0);
            } else {
                unreachable;
            };

            try file.stat(buffer);

            return 0;
        },
        .FileIoControl => {
            const file = process.files.get(frame.rdi) orelse return error.BadFileDescriptor;
            const result = file.vnode.ioctl(frame.rsi, frame.rdx);

            return switch (result) {
                .ok => |value| value,
                .err => |err| errnoToError(@truncate(u16, err)),
            };
        },
        .FileReadDir => {
            const file = process.files.get(frame.rdi) orelse return error.BadFileDescriptor;
            const buffer = try process.validateBuffer([]u8, frame.rsi, frame.rdx);
            const bytes_read = try file.vnode.readDir(buffer, &file.offset);

            return bytes_read;
        },
        .FileChdir => {
            const path = try process.validateSentinel(u8, 0, frame.rdi);
            const vnode = try vfs.resolve(process.cwd, path, 0);

            if (vnode.kind != .Directory) {
                return error.NotDir;
            }

            process.cwd = vnode;
            return 0;
        },
        .MemMap => {
            // All stuff that we don't support (currently) :/
            // I'm not sure whether PROT_NONE is used anywhere..?
            if (frame.rdx == 0 or frame.rdx & ~all_prot != 0 or frame.r10 & ~all_flags != 0) {
                return error.InvalidArgument;
            }

            // And I definitely don't support file mappings at the moment.
            if (frame.r8 != @bitCast(u64, @intCast(i64, -1)) or frame.r9 != 0) {
                return error.InvalidArgument;
            }

            // Also the length has to be non zero :)
            if (frame.rsi == 0) {
                return error.InvalidArgument;
            }

            // const vnode = if (frame.r8 != -1) process.files.get(frame.r8) orelse return error.BadFileDescriptor else null;
            const address = try process.address_space.mmap(frame.rdi, frame.rsi, frame.rdx, frame.r10, null, frame.r9);

            return address;
        },
        .MemUnmap => {
            logger.debug("Attempt to unmap {} byte(s) at 0x{X}", .{ frame.rdi, frame.rsi });

            return 0;
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
