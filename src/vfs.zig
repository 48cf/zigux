const logger = std.log.scoped(.vfs);

const root = @import("root");
const std = @import("std");

const abi = @import("abi.zig");
const tar = @import("tar.zig");
const limine = @import("limine.zig");
const mutex = @import("mutex.zig");
const dev_fs = @import("vfs/dev_fs.zig");
const ram_fs = @import("vfs/ram_fs.zig");

pub const OomError = error{
    OutOfMemory,
};

pub const NotDirError = error{
    NotDir,
};

pub const OpenError = std.os.OpenError || OomError;
pub const ReadError = std.os.PReadError || OomError;
pub const ReadDirError = ReadError || NotDirError;
pub const WriteError = std.os.PWriteError || OomError;
pub const SymlinkError = std.os.SymLinkError || OomError;
pub const IoctlResult = union(enum) { ok: usize, err: usize };
pub const StatError = std.os.FStatAtError || OomError;

pub const VNodeVTable = struct {
    open: ?fn (self: *VNode, name: []const u8, flags: usize) OpenError!*VNode = null,
    read: ?fn (self: *VNode, buffer: []u8, offset: usize) ReadError!usize = null,
    read_dir: ?fn (self: *VNode, buffer: []u8, offset: *usize) ReadDirError!usize = null,
    write: ?fn (self: *VNode, buffer: []const u8, offset: usize) WriteError!usize = null,
    insert: ?fn (self: *VNode, child: *VNode) OomError!void = null,
    ioctl: ?fn (self: *VNode, request: u64, arg: u64) IoctlResult = null,
    stat: ?fn (self: *VNode, buffer: *abi.stat) StatError!void = null,
};

pub const VNodeKind = enum {
    File,
    Directory,
    Symlink,
    CharaterDevice,
    BlockDevice,
};

pub const VNode = struct {
    vtable: *const VNodeVTable,
    filesystem: *FileSystem,
    mounted_vnode: ?*VNode = null,
    kind: VNodeKind = undefined,
    parent: ?*VNode = null,
    name: ?[]const u8 = null,
    symlink_target: ?[]const u8 = null,
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

    pub fn read(self: *VNode, buffer: []u8, offset: usize) !usize {
        const vnode = self.getEffectiveVNode();

        if (vnode.vtable.read) |fun| {
            return fun(vnode, buffer, offset);
        } else if (vnode.kind == .Symlink) {
            const read_length = std.math.min(buffer.len, vnode.symlink_target.?.len);

            std.mem.copy(u8, buffer[0..read_length], vnode.symlink_target.?);

            return read_length;
        } else {
            return error.NotImplemented;
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

    pub fn write(self: *VNode, buffer: []const u8, offset: usize) !usize {
        const vnode = self.getEffectiveVNode();

        if (vnode.vtable.write) |fun| {
            return fun(vnode, buffer, offset);
        } else if (vnode.kind == .Symlink) {
            return error.NotOpenForWriting;
        } else {
            return error.NotImplemented;
        }
    }

    pub fn writeAll(self: *VNode, buffer: []const u8, offset: usize) !void {
        var buf = buffer;
        var off = offset;

        while (buf.len > 0) {
            const written = try self.write(buf, off);

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

    pub fn ioctl(self: *VNode, request: u64, arg: u64) IoctlResult {
        const vnode = self.getEffectiveVNode();

        if (vnode.vtable.ioctl) |fun| {
            return fun(vnode, request, arg);
        } else {
            return .{ .err = abi.ENOSYS };
        }
    }

    pub fn stat(self: *VNode, buffer: *abi.stat) !void {
        const vnode = self.getEffectiveVNode();

        if (vnode.vtable.stat) |fun| {
            return fun(vnode, buffer);
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

fn formatPath(node: *VNode, writer: anytype) @TypeOf(writer).Error!void {
    if (node.parent) |parent| {
        try formatPath(parent, writer);
        try writer.writeByte('/');
    }

    try writer.writeAll(node.name.?);
}

pub const VNodePath = struct {
    node: *VNode,

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
        self.offset +%= @bitCast(u64, offset);
    }

    fn getPosFn(self: *VNodeStream) GetSeekPosError!u64 {
        return self.offset;
    }

    fn getEndPosFn(self: *VNodeStream) GetSeekPosError!u64 {
        _ = self;

        return 0;
    }

    fn read(self: *VNodeStream, buffer: []u8) ReaderError!usize {
        return self.node.read(buffer, self.offset);
    }

    pub fn seekableStream(self: *VNodeStream) SeekableStream {
        return .{ .context = self };
    }

    pub fn reader(self: *VNodeStream) Reader {
        return .{ .context = self };
    }
};

pub const FileSystemVTable = struct {
    create_file: ?fn (self: *FileSystem) OomError!*VNode,
    create_dir: ?fn (self: *FileSystem) OomError!*VNode,
    create_symlink: ?fn (self: *FileSystem, target: []const u8) OomError!*VNode,
};

pub const FileSystem = struct {
    vtable: *const FileSystemVTable,
    case_sensitive: bool,
    name: []const u8,

    pub fn createFile(self: *FileSystem, name: []const u8) !*VNode {
        if (self.vtable.create_file) |fun| {
            const node = try fun(self);

            node.kind = .File;
            node.name = name;

            return node;
        } else {
            return error.NotImplemented;
        }
    }

    pub fn createDir(self: *FileSystem, name: []const u8) !*VNode {
        if (self.vtable.create_dir) |fun| {
            const node = try fun(self);

            node.kind = .Directory;
            node.name = name;

            return node;
        } else {
            return error.NotImplemented;
        }
    }

    pub fn createSymlink(self: *FileSystem, name: []const u8, target: []const u8) !*VNode {
        if (self.vtable.create_symlink) |fun| {
            const node = try fun(self, target);

            node.kind = .Symlink;
            node.name = name;

            return node;
        } else {
            return error.NotImplemented;
        }
    }
};

var root_vnode: ?*VNode = null;

pub fn init(modules_res: *limine.Modules.Response) !void {
    const root_node = try ram_fs.init("", null);

    root_vnode = root_node;

    const bin_dir = try root_node.filesystem.createDir("bin");
    const dev_dir = try root_node.filesystem.createDir("dev");
    const lib_dir = try root_node.filesystem.createDir("lib");
    const root_dir = try root_node.filesystem.createDir("root");
    const sys_dir = try root_node.filesystem.createDir("sys");

    try root_node.insert(bin_dir);
    try root_node.insert(dev_dir);
    try root_node.insert(lib_dir);
    try root_node.insert(root_dir);
    try root_node.insert(sys_dir);

    // Initalize /dev
    dev_dir.mount(try dev_fs.init("dev", null));

    // Initialize /sys
    const modules_dir = try sys_dir.filesystem.createDir("modules");

    try sys_dir.insert(modules_dir);

    for (modules_res.modules[0..modules_res.module_count]) |module| {
        const name = std.fs.path.basename(std.mem.span(module.path));
        const module_file = try modules_dir.filesystem.createFile(name);
        const data_blob = module.address[0..module.size];

        try module_file.writeAll(data_blob, 0);
        try modules_dir.insert(module_file);

        if (std.mem.endsWith(u8, name, ".tar")) {
            var files: usize = 0;
            var total_size: usize = 0;
            var iterator = tar.iterate(data_blob);

            while (try iterator.next()) |file| {
                files += 1;

                switch (file.kind) {
                    .Normal => {
                        const file_node = try resolve(root_node, file.name, abi.O_CREAT);
                        try file_node.writeAll(file.data, 0);

                        total_size += file.data.len;
                    },
                    .SymbolicLink => continue, // We do one more loop later to fix those :)
                    .Directory => _ = try resolve(root_node, file.name, abi.O_CREAT),
                    else => logger.warn("Unhandled file {s} of type {}", .{ file.name, file.kind }),
                }
            }

            iterator = tar.iterate(data_blob);

            while (try iterator.next()) |file| {
                files += 1;

                switch (file.kind) {
                    .SymbolicLink => {
                        const parent_path = std.fs.path.dirname(file.name) orelse unreachable;
                        const parent_node = try resolve(root_node, parent_path, abi.O_CREAT);
                        const link_node = try parent_node.filesystem.createSymlink(std.fs.path.basename(file.name), file.link);

                        try parent_node.insert(link_node);
                    },
                    else => continue,
                }
            }

            logger.info("Loaded {} ({}KiB) files from {}", .{ files, total_size / 1024, module_file.getFullPath() });
        }
    }
}

pub fn resolve(cwd: ?*VNode, path: []const u8, flags: u64) (OpenError || error{ NotImplemented, NotFound })!*VNode {
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

                            if (flags & abi.O_CREAT != 0) {
                                if (path[path.len - 1] == '/') {
                                    const node = try fs.createDir(component);

                                    try next.insert(node);

                                    break :blk node;
                                } else {
                                    const node = try fs.createFile(component);

                                    try next.insert(node);

                                    break :blk node;
                                }
                            } else {
                                return error.NotFound;
                            }
                        },
                        else => return err,
                    }
                };
            }

            if (flags & abi.O_NOFOLLOW == 0 and next_node.?.kind == .Symlink) {
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
                return error.NotFound;
            }
        }
    }

    return next;
}
