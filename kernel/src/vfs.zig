const logger = std.log.scoped(.vfs);

const root = @import("root");
const std = @import("std");
const limine = @import("limine");

const abi = @import("abi.zig");
const tar = @import("tar.zig");
const mutex = @import("mutex.zig");
const utils = @import("utils.zig");
const dev_fs = @import("vfs/dev_fs.zig");
const ram_fs = @import("vfs/ram_fs.zig");

const RingWaitQueue = @import("containers/ring_buffer.zig").RingWaitQueue;

pub const OomError = error{OutOfMemory};
pub const NotDirError = error{NotDir};
pub const FaultError = error{AddressInaccessible};

pub const OpenError = std.os.OpenError || OomError;
pub const ReadError = std.os.PReadError || OomError;
pub const ReadDirError = ReadError || NotDirError;
pub const WriteError = std.os.PWriteError || OomError;
pub const InsertError = std.os.MakeDirError || OomError;
pub const SymlinkError = std.os.SymLinkError || OomError;
pub const IoctlError = error{ InvalidArgument, NoDevice } || FaultError;
pub const StatError = std.os.FStatAtError || OomError;

pub const VNodeVTable = struct {
    open: ?*const fn (self: *VNode, name: []const u8, flags: usize) OpenError!*VNode = null,
    close: ?*const fn (self: *VNode) void = null,
    read: ?*const fn (self: *VNode, buffer: []u8, offset: usize, flags: usize) ReadError!usize = null,
    read_dir: ?*const fn (self: *VNode, buffer: []u8, offset: *usize) ReadDirError!usize = null,
    write: ?*const fn (self: *VNode, buffer: []const u8, offset: usize, flags: usize) WriteError!usize = null,
    insert: ?*const fn (self: *VNode, child: *VNode) InsertError!void = null,
    ioctl: ?*const fn (self: *VNode, request: u64, arg: u64) IoctlError!u64 = null,
    stat: ?*const fn (self: *VNode, buffer: *abi.C.stat) StatError!void = null,
};

pub const VNodeKind = enum {
    File,
    Directory,
    Symlink,
    CharacterDevice,
    BlockDevice,
    Fifo,
    Socket,
};

pub const VNode = struct {
    vtable: *const VNodeVTable,
    filesystem: *FileSystem,
    mounted_vnode: ?*VNode = null,
    kind: VNodeKind,
    parent: ?*VNode = null,
    name: ?[]const u8 = null,
    symlink_target: ?[]const u8 = null,
    inode: u64 = 0,
    lock: mutex.AtomicMutex = .{},

    fn getEffectiveVNode(self: *VNode) *VNode {
        return self.mounted_vnode orelse self;
    }

    fn getEffectiveFs(self: *VNode) *FileSystem {
        return self.getEffectiveVNode().filesystem;
    }

    pub fn open(self: *VNode, name: []const u8, flags: usize) !*VNode {
        const vnode = self.getEffectiveVNode();

        if (vnode.vtable.open) |fun| {
            return fun(vnode, name, flags);
        } else {
            return error.NotImplemented;
        }
    }

    pub fn close(self: *VNode) void {
        const vnode = self.getEffectiveVNode();

        if (vnode.vtable.close) |fun| {
            return fun(vnode);
        }
    }

    pub fn read(self: *VNode, buffer: []u8, offset: usize, flags: usize) !usize {
        const vnode = self.getEffectiveVNode();

        if (vnode.vtable.read) |fun| {
            return fun(vnode, buffer, offset, flags);
        } else if (vnode.kind == .Symlink) {
            const read_length = @min(buffer.len, vnode.symlink_target.?.len);

            @memcpy(buffer[0..read_length], vnode.symlink_target.?);

            return read_length;
        } else {
            return error.NotImplemented;
        }
    }

    pub fn readAll(self: *VNode, buffer: []u8, offset: usize, flags: usize) !void {
        var buf = buffer;
        var off = offset;

        while (buf.len > 0) {
            const read_amount = try self.read(buf, off, flags);

            if (read_amount == 0) {
                return error.EndOfStream;
            }

            buf = buf[read_amount..];
            off += read_amount;
        }
    }

    pub fn readDir(self: *VNode, buffer: []u8, offset: *usize) !usize {
        const vnode = self.getEffectiveVNode();

        if (vnode.vtable.read_dir) |fun| {
            return fun(vnode, buffer, offset);
        } else {
            return error.NotImplemented;
        }
    }

    pub fn write(self: *VNode, buffer: []const u8, offset: usize, flags: usize) !usize {
        const vnode = self.getEffectiveVNode();

        if (vnode.vtable.write) |fun| {
            return fun(vnode, buffer, offset, flags);
        } else if (vnode.kind == .Symlink) {
            return error.NotOpenForWriting;
        } else {
            return error.NotImplemented;
        }
    }

    pub fn writeAll(self: *VNode, buffer: []const u8, offset: usize, flags: usize) !void {
        var buf = buffer;
        var off = offset;

        while (buf.len > 0) {
            const written = try self.write(buf, off, flags);

            if (written == 0) {
                return error.EndOfStream;
            }

            buf = buf[written..];
            off += written;
        }
    }

    pub fn insert(self: *VNode, child: *VNode) !void {
        self.lock.lock();
        defer self.lock.unlock();

        std.debug.assert(child.name != null);

        const vnode = self.getEffectiveVNode();

        if (vnode.vtable.insert) |fun| {
            const old_parent = child.parent;

            errdefer child.parent = old_parent;

            child.parent = vnode;

            return fun(vnode, child);
        } else {
            @panic("An insert operation is required");
        }
    }

    pub fn ioctl(self: *VNode, request: u64, arg: u64) !u64 {
        const vnode = self.getEffectiveVNode();

        if (vnode.vtable.ioctl) |fun| {
            return fun(vnode, request, arg);
        } else {
            return error.NoDevice;
        }
    }

    pub fn stat(self: *VNode, buffer: *abi.C.stat) !void {
        const vnode = self.getEffectiveVNode();

        if (vnode.vtable.stat) |fun| {
            return fun(vnode, buffer);
        } else if (vnode.kind == .Symlink) {
            const target = self.symlink_target.?;

            buffer.* = std.mem.zeroes(abi.C.stat);
            buffer.st_mode |= 0o777 | abi.C.S_IFLNK;
            buffer.st_size = @as(c_long, @intCast(target.len));
            buffer.st_blksize = std.mem.page_size;
            buffer.st_blocks = @as(c_long, @intCast(std.mem.alignForward(usize, target.len, std.mem.page_size) / std.mem.page_size));
        } else {
            return error.NotImplemented;
        }
    }

    pub fn mount(self: *VNode, other: *VNode) void {
        self.lock.lock();
        defer self.lock.unlock();

        std.debug.assert(self.mounted_vnode == null);
        std.debug.assert(other.parent == null);

        self.mounted_vnode = other;
        other.parent = self.parent;
    }

    pub fn getFullPath(self: *VNode) VNodePath {
        return .{ .node = self };
    }

    pub fn stream(self: *VNode) VNodeStream {
        return .{ .node = self, .offset = 0 };
    }
};

pub const VNodePath = struct {
    node: *VNode,

    fn formatPath(node: *VNode, writer: anytype) @TypeOf(writer).Error!void {
        if (node.parent) |parent| {
            try formatPath(parent, writer);
            try writer.writeByte('/');
        }

        try writer.writeAll(node.name.?);
    }

    pub fn format(
        value: *const VNodePath,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try formatPath(value.node, writer);
    }
};

pub const VNodeStream = struct {
    node: *VNode,
    offset: u64,

    pub const ReaderError = ReadError || error{NotImplemented};
    pub const SeekError = error{};
    pub const GetSeekPosError = error{};

    pub const SeekableStream = std.io.SeekableStream(
        *VNodeStream,
        SeekError,
        GetSeekPosError,
        VNodeStream.seekTo,
        VNodeStream.seekBy,
        VNodeStream.getPosFn,
        VNodeStream.getEndPosFn,
    );
    pub const Reader = std.io.Reader(
        *VNodeStream,
        ReaderError,
        VNodeStream.read,
    );

    fn seekTo(self: *VNodeStream, offset: u64) SeekError!void {
        self.offset = offset;
    }

    fn seekBy(self: *VNodeStream, offset: i64) SeekError!void {
        self.offset +%= @as(u64, @bitCast(offset));
    }

    fn getPosFn(self: *VNodeStream) GetSeekPosError!u64 {
        return self.offset;
    }

    fn getEndPosFn(self: *VNodeStream) GetSeekPosError!u64 {
        _ = self;

        return 0;
    }

    fn read(self: *VNodeStream, buffer: []u8) ReaderError!usize {
        // TODO: Figure out what flags to pass in here..?
        return self.node.read(buffer, self.offset, 0);
    }

    pub fn seekableStream(self: *VNodeStream) SeekableStream {
        return .{ .context = self };
    }

    pub fn reader(self: *VNodeStream) Reader {
        return .{ .context = self };
    }
};

const Pipe = struct {
    vnode: VNode,
    buffer: RingWaitQueue(u8, 1024) = .{},
    closed: bool = false,

    const vtable: VNodeVTable = .{
        .close = Pipe.close,
        .read = Pipe.read,
        .write = Pipe.write,
    };

    fn close(vnode: *VNode) void {
        const self = @fieldParentPtr(Pipe, "vnode", vnode);

        self.closed = true;
        self.buffer.semaphore.release(1);
    }

    fn read(vnode: *VNode, buffer: []u8, offset: usize, flags: usize) ReadError!usize {
        _ = offset;
        _ = flags;

        const self = @fieldParentPtr(Pipe, "vnode", vnode);

        if (!self.closed) {
            while (true) {
                self.buffer.semaphore.acquire(1);

                if (self.closed) {
                    return 0;
                }

                buffer[0] = self.buffer.buffer.pop() orelse continue;

                break;
            }

            for (buffer[1..], 1..) |*byte, i| {
                byte.* = self.buffer.buffer.pop() orelse return i + 1;
            }
        } else {
            for (buffer, 0..) |*byte, i| {
                byte.* = self.buffer.buffer.pop() orelse return i;
            }
        }

        return buffer.len;
    }

    fn write(vnode: *VNode, buffer: []const u8, offset: usize, flags: usize) WriteError!usize {
        _ = offset;
        _ = flags;

        const self = @fieldParentPtr(Pipe, "vnode", vnode);

        if (self.closed) {
            return error.BrokenPipe;
        }

        for (buffer, 0..) |byte, i| {
            if (!self.buffer.push(byte)) {
                return if (i == 0) error.WouldBlock else i;
            }
        }

        return buffer.len;
    }
};

const SliceBackedFile = struct {
    vnode: VNode,
    data: []const u8,

    const slice_backed_file_vtable: VNodeVTable = .{
        .read = SliceBackedFile.read,
        .write = SliceBackedFile.write,
        .stat = SliceBackedFile.stat,
    };

    fn read(vnode: *VNode, buffer: []u8, offset: usize, flags: usize) ReadError!usize {
        const self = @fieldParentPtr(SliceBackedFile, "vnode", vnode);

        _ = flags;

        if (offset >= self.data.len) {
            return 0;
        }

        const bytes_read = @min(buffer.len, self.data.len - offset);

        @memcpy(buffer[0..bytes_read], self.data[offset .. offset + bytes_read]);

        return bytes_read;
    }

    fn write(vnode: *VNode, buffer: []const u8, offset: usize, flags: usize) WriteError!usize {
        _ = vnode;
        _ = buffer;
        _ = offset;
        _ = flags;

        return error.NotOpenForWriting;
    }

    fn stat(vnode: *VNode, buffer: *abi.C.stat) StatError!void {
        const self = @fieldParentPtr(SliceBackedFile, "vnode", vnode);

        buffer.* = std.mem.zeroes(abi.C.stat);
        buffer.st_mode = 0o777 | abi.C.S_IFREG;
        buffer.st_size = @as(c_long, @intCast(self.data.len));
        buffer.st_blksize = std.mem.page_size;
        buffer.st_blocks = @as(c_long, @intCast(std.mem.alignForward(usize, self.data.len, std.mem.page_size) / std.mem.page_size));
    }
};

pub const FileSystemVTable = struct {
    create_file: ?*const fn (self: *FileSystem) OomError!*VNode,
    create_dir: ?*const fn (self: *FileSystem) OomError!*VNode,
    create_symlink: ?*const fn (self: *FileSystem, target: []const u8) OomError!*VNode,
    allocate_inode: *const fn (self: *FileSystem) OomError!u64,
};

pub const FileSystem = struct {
    vtable: *const FileSystemVTable,
    case_sensitive: bool,
    name: []const u8,

    pub fn createFile(self: *FileSystem, name: []const u8) !*VNode {
        if (self.vtable.create_file) |fun| {
            const inode = try self.vtable.allocate_inode(self);
            const node = try fun(self);

            node.kind = .File;
            node.name = try root.allocator.dupe(u8, name);
            node.inode = inode;

            return node;
        } else {
            return error.NotImplemented;
        }
    }

    pub fn createDir(self: *FileSystem, name: []const u8) !*VNode {
        if (self.vtable.create_dir) |fun| {
            const inode = try self.vtable.allocate_inode(self);
            const node = try fun(self);

            node.kind = .Directory;
            node.name = try root.allocator.dupe(u8, name);
            node.inode = inode;

            return node;
        } else {
            return error.NotImplemented;
        }
    }

    pub fn createSymlink(self: *FileSystem, name: []const u8, target: []const u8) !*VNode {
        if (self.vtable.create_symlink) |fun| {
            const inode = try self.vtable.allocate_inode(self);
            const node = try fun(self, target);

            node.kind = .Symlink;
            node.name = try root.allocator.dupe(u8, name);
            node.inode = inode;

            return node;
        } else {
            return error.NotImplemented;
        }
    }
};

var root_vnode: ?*VNode = null;

fn createSliceBackedFile(name: []const u8, data: []const u8) !*VNode {
    const file = try root.allocator.create(SliceBackedFile);

    file.* = .{
        .vnode = .{
            .vtable = &SliceBackedFile.slice_backed_file_vtable,
            .filesystem = undefined,
            .kind = .File,
            .name = name,
        },
        .data = data,
    };

    return &file.vnode;
}

pub fn init(modules_res: *limine.ModuleResponse) !void {
    const root_node = try ram_fs.init("", null);

    root_vnode = root_node;

    for (modules_res.modules()) |module| {
        const name = std.fs.path.basename(std.mem.span(module.path));
        const module_file = try createSliceBackedFile(name, module.data());

        if (std.mem.endsWith(u8, name, ".tar")) {
            var files: usize = 0;
            var total_size: usize = 0;
            var iterator = tar.iterate(module.data());

            while (try iterator.next()) |file| {
                files += 1;

                switch (file.kind) {
                    .Normal => {
                        const parent_path = std.fs.path.dirname(file.name) orelse "/";
                        const parent = try resolve(root_node, parent_path, abi.C.O_CREAT);
                        const file_node = try createSliceBackedFile(std.fs.path.basename(file.name), file.data);

                        try parent.insert(file_node);

                        total_size += file.data.len;
                    },
                    .SymbolicLink => {
                        const parent_path = std.fs.path.dirname(file.name) orelse "/";
                        const parent = try resolve(root_node, parent_path, abi.C.O_CREAT);
                        const link_node = try parent.filesystem.createSymlink(std.fs.path.basename(file.name), file.link);

                        try parent.insert(link_node);
                    },
                    .Directory => {
                        _ = try resolve(root_node, file.name, abi.C.O_CREAT | abi.C.O_DIRECTORY);
                    },
                    else => logger.warn("Unhandled file {s} of type {}", .{ file.name, file.kind }),
                }
            }

            logger.info("Loaded {} ({}KiB) files from {}", .{ files, total_size / 1024, module_file.getFullPath() });
        }
    }

    // Initalize /dev
    const dev_dir = try resolve(root_node, "dev", abi.C.O_CREAT | abi.C.O_DIRECTORY);

    dev_dir.mount(try dev_fs.init("dev", null));
}

pub fn resolve(cwd: ?*VNode, path: []const u8, flags: u64) (OpenError || InsertError || error{NotImplemented})!*VNode {
    if (cwd == null) {
        std.debug.assert(std.fs.path.isAbsolute(path));

        return resolve(root_vnode.?, path[1..], flags);
    }

    var next = if (std.fs.path.isAbsolute(path)) root_vnode.? else cwd.?;
    var iter = std.mem.split(u8, path, std.fs.path.sep_str);

    if (path.len > 0) {
        while (iter.next()) |component| {
            var next_node: ?*VNode = null;

            if (component.len == 0 or std.mem.eql(u8, component, ".")) {
                continue;
            } else if (std.mem.eql(u8, component, "..")) {
                next_node = next.parent orelse next_node;
            } else {
                next_node = next.open(component, 0) catch |err| blk: {
                    switch (err) {
                        error.FileNotFound => {
                            const fs = next.getEffectiveFs();

                            if (flags & abi.C.O_CREAT != 0) {
                                if (flags & abi.C.O_DIRECTORY != 0 or iter.rest().len > 0) {
                                    const node = try fs.createDir(component);
                                    try next.insert(node);
                                    break :blk node;
                                } else {
                                    const node = try fs.createFile(component);
                                    try next.insert(node);
                                    break :blk node;
                                }
                            } else {
                                return error.FileNotFound;
                            }
                        },
                        else => return err,
                    }
                };
            }

            if (flags & abi.C.O_NOFOLLOW == 0 and next_node.?.kind == .Symlink) {
                const new_node = next_node.?;
                const target = new_node.symlink_target.?;

                if (std.fs.path.isAbsolute(target)) {
                    next_node = try resolve(null, target, 0);
                } else {
                    next_node = try resolve(new_node.parent, target, 0);
                }

                if (next_node.? == new_node) {
                    return error.SymLinkLoop;
                }
            }

            if (next_node) |new_node| {
                next = new_node;
            } else {
                return error.FileNotFound;
            }
        }
    }

    return next;
}

pub fn createPipe() !*VNode {
    const pipe = try root.allocator.create(Pipe);

    pipe.* = .{
        .vnode = .{
            .vtable = &Pipe.vtable,
            .filesystem = undefined,
            .kind = .Fifo,
            .name = "(pipe)",
        },
    };

    return &pipe.vnode;
}
