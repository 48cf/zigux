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

const FileDescriptor = struct {
    vnode: *vfs.VNode,
    offset: usize,
};

const FileTable = struct {
    files: std.AutoArrayHashMapUnmanaged(u64, *FileDescriptor) = .{},
    fd_counter: u64 = 0,

    fn findFreeFd(self: *FileTable) u64 {
        var i = self.fd_counter;

        while (true) : (i += 1) {
            if (!self.files.contains(i)) {
                self.fd_counter = i + 1;
                return i;
            }
        }
    }

    pub fn fork(self: *FileTable) !FileTable {
        var new_table = FileTable{ .fd_counter = self.fd_counter };
        var iterator = self.files.iterator();

        while (iterator.next()) |entry| {
            try new_table.files.put(root.allocator, entry.key_ptr.*, entry.value_ptr.*);
        }

        return new_table;
    }

    pub fn duplicate(self: *FileTable, desc: *FileDescriptor, new_fd_opt: ?u64) !u64 {
        const fd = new_fd_opt orelse self.findFreeFd();

        try self.files.put(root.allocator, fd, desc);

        return fd;
    }

    pub fn insertAt(self: *FileTable, fd: u64, vnode: *vfs.VNode) !void {
        const desc = try root.allocator.create(FileDescriptor);

        errdefer root.allocator.destroy(desc);

        desc.* = .{
            .vnode = vnode,
            .offset = 0,
        };

        self.fd_counter = std.math.max(fd + 1, self.fd_counter);

        try self.files.put(root.allocator, fd, desc);
    }

    pub fn insert(self: *FileTable, vnode: *vfs.VNode) !u64 {
        const fd = self.findFreeFd();

        try self.insertAt(fd, vnode);

        return fd;
    }

    pub fn get(self: *FileTable, fd: u64) ?*FileDescriptor {
        if (self.files.getPtr(fd)) |result| {
            return result.*;
        } else {
            return null;
        }
    }

    pub fn remove(self: *FileTable, fd: u64) bool {
        if (fd <= self.fd_counter) {
            self.fd_counter = fd;
        }

        defer _ = self.files.swapRemove(fd);

        if (self.get(fd)) |file| {
            file.vnode.close();
            return true;
        }

        return false;
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
    .{ error.InvalidElfMagic, abi.ENOEXEC },
};

fn errnoToError(errno: u16) anyerror {
    inline for (errorTypeMap) |pair| {
        if (pair[1] == errno) {
            return pair[0];
        }
    }

    return error.Unexpected;
}

fn syscallNumberToString(syscall_num: u64) ?[]const u8 {
    @setEvalBranchQuota(10000);

    inline for (@typeInfo(abi).Struct.decls) |decl| {
        if (comptime std.mem.startsWith(u8, decl.name, "SYS_")) {
            if (@field(abi, decl.name) == syscall_num) {
                return decl.name;
            }
        }
    }

    return null;
}

inline fn errorToErrno(err: anyerror) u16 {
    inline for (errorTypeMap) |pair| {
        if (pair[0] == err) {
            return pair[1];
        }
    }

    std.debug.panicExtra(@errorReturnTrace(), "Unhandled error: {}", .{err});
}

pub fn syscallHandler(frame: *interrupts.InterruptFrame) void {
    const syscall_name = syscallNumberToString(frame.rax) orelse "???";

    logger.debug(
        "Syscall {s}: {{ 0x{X}, 0x{X}, 0x{X}, 0x{X}, 0x{X}, 0x{X} }}",
        .{ syscall_name, frame.rdi, frame.rsi, frame.rdx, frame.r10, frame.r8, frame.r9 },
    );

    frame.rax = syscallHandlerImpl(frame) catch |err| {
        logger.debug("Syscall {s} returned {}", .{ syscall_name, err });

        const errno = errorToErrno(err);

        frame.rax = @bitCast(u64, @as(i64, -@bitCast(i16, errno)));

        return;
    } orelse return;

    logger.debug("Syscall {s} returned 0x{X}", .{ syscall_name, frame.rax });
}

fn syscallHandlerImpl(frame: *interrupts.InterruptFrame) !?u64 {
    const cpu_info = per_cpu.get();
    const process = cpu_info.currentProcess().?;

    return switch (frame.rax) {
        abi.SYS_PROC_EXIT => {
            cpu_info.thread = null;

            scheduler.exitProcess(process, @truncate(u8, frame.rdi));
            scheduler.reschedule(frame);

            return null;
        },
        abi.SYS_PROC_LOG => {
            const buffer = try process.validateSentinel(u8, 0, frame.rdi);
            const length = std.mem.len(buffer);

            logger.info("{s}", .{buffer[0..length]});

            return 0;
        },
        abi.SYS_PROC_ARCH_CTL => {
            switch (frame.rdi) {
                abi.ARCH_SET_FS => {
                    arch.Msr.fs_base.write(frame.rsi);

                    return 0;
                },
                else => return error.InvalidArgument,
            }
        },
        abi.SYS_PROC_GET_PID => return process.pid,
        abi.SYS_PROC_GET_PPID => return process.parent,
        abi.SYS_PROC_FORK => {
            const forked_process = try scheduler.forkProcess(process);
            const new_thread = try scheduler.spawnThreadWithoutStack(forked_process);

            new_thread.regs = frame.*;
            new_thread.regs.rax = 0;

            scheduler.enqueue(new_thread);

            return forked_process.pid;
        },
        abi.SYS_PROC_EXEC => {
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
        abi.SYS_PROC_WAIT => {
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
        abi.SYS_FILE_OPEN => {
            const path = try process.validateSentinel(u8, 0, frame.rdi);
            const vnode = try vfs.resolve(process.cwd, path, frame.rsi);

            return try process.files.insert(vnode);
        },
        abi.SYS_FILE_CLOSE => {
            if (process.files.remove(frame.rdi)) {
                return 0;
            } else {
                return error.BadFileDescriptor;
            }
        },
        abi.SYS_FILE_READ => {
            const file = process.files.get(frame.rdi) orelse return error.BadFileDescriptor;
            const buffer = try process.validateBuffer([]u8, frame.rsi, frame.rdx);
            const bytes_read = try file.vnode.read(buffer, file.offset, 0); // TODO: Pass read flags there

            file.offset += bytes_read;

            return bytes_read;
        },
        abi.SYS_FILE_WRITE => {
            const file = process.files.get(frame.rdi) orelse return error.BadFileDescriptor;
            const buffer = try process.validateBuffer([]const u8, frame.rsi, frame.rdx);
            const bytes_written = try file.vnode.write(buffer, file.offset, 0); // TODO: Pass write flags there

            file.offset += bytes_written;

            return bytes_written;
        },
        abi.SYS_FILE_SEEK => {
            const file = process.files.get(frame.rdi) orelse return error.BadFileDescriptor;

            switch (frame.rdx) {
                abi.SEEK_SET => file.offset = frame.rsi,
                abi.SEEK_CUR => file.offset +%= frame.rsi,
                abi.SEEK_END => unreachable,
                else => return error.InvalidArgument,
            }

            return file.offset;
        },
        abi.SYS_FILE_GET_CWD => {
            var string_buf = try process.validateBuffer([]u8, frame.rdi, frame.rsi);
            var buffer = std.io.fixedBufferStream(string_buf);
            var writer = buffer.writer();

            try writer.print("{}", .{process.cwd.getFullPath()});
            try writer.writeByte(0);

            return std.mem.len(@ptrCast([*:0]const u8, string_buf));
        },
        abi.SYS_FILE_STAT_FD, abi.SYS_FILE_STAT_PATH => {
            const buffer = try process.validatePointer(abi.stat, frame.rsi);
            const file = if (frame.rax == abi.SYS_FILE_STAT_FD) blk: {
                const fd = process.files.get(frame.rdi) orelse return error.BadFileDescriptor;
                break :blk fd.vnode;
            } else if (frame.rax == abi.SYS_FILE_STAT_PATH) blk: {
                const path = try process.validateSentinel(u8, 0, frame.rdi);
                break :blk try vfs.resolve(process.cwd, path, frame.rdx | abi.O_NOFOLLOW);
            } else {
                unreachable;
            };

            try file.stat(buffer);

            return 0;
        },
        abi.SYS_FILE_IO_CONTROL => {
            const file = process.files.get(frame.rdi) orelse return error.BadFileDescriptor;

            return try file.vnode.ioctl(frame.rsi, frame.rdx);
        },
        abi.SYS_FILE_READ_DIR => {
            const file = process.files.get(frame.rdi) orelse return error.BadFileDescriptor;
            const buffer = try process.validateBuffer([]u8, frame.rsi, frame.rdx);
            const bytes_read = try file.vnode.readDir(buffer, &file.offset);

            return bytes_read;
        },
        abi.SYS_FILE_CHDIR => {
            const path = try process.validateSentinel(u8, 0, frame.rdi);
            const vnode = try vfs.resolve(process.cwd, path, 0);

            if (vnode.kind != .Directory) {
                return error.NotDir;
            }

            process.cwd = vnode;
            return 0;
        },
        abi.SYS_FILE_DUP => {
            const file = process.files.get(frame.rdi) orelse return error.BadFileDescriptor;
            const new_fd = try process.files.duplicate(file, null);

            return new_fd;
        },
        abi.SYS_FILE_DUP2 => {
            const file = process.files.get(frame.rdi) orelse return error.BadFileDescriptor;

            if (frame.rdi != frame.rdx) {
                _ = try process.files.duplicate(file, frame.rdx);
            }

            return frame.rdx;
        },
        abi.SYS_FILE_FCNTL => {
            _ = process.files.get(frame.rdi) orelse return error.BadFileDescriptor;

            logger.debug(
                "Attempt to call fcntl on fd {} with request: 0x{X} and argument: 0x{X}",
                .{ frame.rdi, frame.rsi, frame.rdx },
            );

            return error.NotImplemented;
        },
        abi.SYS_FILE_PIPE => {
            const buffer = try process.validateBuffer([]i32, frame.rdi, 2);
            const file = try vfs.createPipe();

            const read_end = try process.files.insert(file);
            const write_end = try process.files.insert(file);

            buffer[0] = @truncate(i32, @bitCast(i64, read_end));
            buffer[1] = @truncate(i32, @bitCast(i64, write_end));

            return 0;
        },
        abi.SYS_MEM_MAP => {
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
        abi.SYS_MEM_UNMAP => {
            logger.debug("Attempt to unmap {} byte(s) at 0x{X}", .{ frame.rdi, frame.rsi });

            return 0;
        },
        else => {
            logger.warn(
                "Unimplemented syscall 0x{X}: {{ 0x{X}, 0x{X}, 0x{X}, 0x{X}, 0x{X}, 0x{X} }}",
                .{ frame.rax, frame.rdi, frame.rsi, frame.rdx, frame.r10, frame.r8, frame.r9 },
            );

            return error.NotImplemented;
        },
    };
}
