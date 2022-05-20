const logger = std.log.scoped(.vfs);

const root = @import("root");
const std = @import("std");

const limine = @import("limine.zig");
const dev_fs = @import("vfs/dev_fs.zig");
const ram_fs = @import("vfs/ram_fs.zig");

pub const OomError = error{OutOfMemory};
pub const OpenError = std.os.OpenError || OomError;
pub const ReadError = std.os.PReadError || OomError;
pub const WriteError = std.os.PWriteError || OomError;
pub const MmapError = std.os.MMapError || OomError;

pub const VNodeVTable = struct {
    open: ?fn (self: *VNode, name: []const u8, flags: usize) OpenError!*VNode,
    read: ?fn (self: *VNode, buffer: []u8, offset: usize) ReadError!usize,
    write: ?fn (self: *VNode, buffer: []const u8, offset: usize) WriteError!usize,
    insert: ?fn (self: *VNode, child: *VNode) OomError!void,
    mmap: ?fn (self: *VNode, offset: usize, flags: usize) MmapError!u64,
    // ioctl: ?fn (self: *Vnode, request: u64, arg: u64) std.os.linux.E!usize,
};

pub const VNodeKind = enum {
    File,
    Directory,
};

pub const VNode = struct {
    vtable: *const VNodeVTable,
    filesystem: *FileSystem,
    mounted_vnode: ?*VNode = null,
    kind: VNodeKind = undefined,
    parent: ?*VNode = null,
    name: ?[]const u8 = null,

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
        } else {
            return error.NotImplemented;
        }
    }

    pub fn write(self: *VNode, buffer: []const u8, offset: usize) !usize {
        const vnode = self.getEffectiveVNode();

        if (vnode.vtable.write) |fun| {
            return fun(vnode, buffer, offset);
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
        std.debug.assert(child.name != null);
        std.debug.assert(child.kind == .File or child.kind == .Directory);

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

    pub fn mmap(self: *VNode, offset: usize, flags: usize) !u64 {
        const vnode = self.getEffectiveVNode();

        if (vnode.vtable.mmap) |fun| {
            return fun(vnode, offset, flags);
        } else {
            return error.NotImplemented;
        }
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
    create_file: ?fn (self: *FileSystem) error{OutOfMemory}!*VNode,
    create_dir: ?fn (self: *FileSystem) error{OutOfMemory}!*VNode,
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
};

var root_vnode: ?*VNode = null;

const assembler_elf = @embedFile("../misc/s3").*;
const assembler_source = @embedFile("../misc/shr.shr").*;

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

    // Initialize /bin and /root
    const s3_file = try root_node.filesystem.createFile("s3");
    const shr_file = try root_node.filesystem.createFile("shr.shr");

    try s3_file.writeAll(&assembler_elf, 0);
    try shr_file.writeAll(&assembler_source, 0);

    try bin_dir.insert(s3_file);
    try root_dir.insert(shr_file);

    // Initalize /dev
    dev_dir.mounted_vnode = try dev_fs.init("dev", root_node);

    // Initialize /sys
    const modules_dir = try sys_dir.filesystem.createDir("modules");

    for (modules_res.modules[0..modules_res.module_count]) |module| {
        const name = std.fs.path.basename(std.mem.span(module.path));
        const module_file = try modules_dir.filesystem.createFile(name);

        try module_file.writeAll(module.address[0..module.size], 0);
        try modules_dir.insert(module_file);
    }

    try sys_dir.insert(modules_dir);
}

pub fn resolve(cwd: ?*VNode, path: []const u8, flags: u64) !*VNode {
    if (cwd == null) {
        std.debug.assert(std.fs.path.isAbsolute(path));

        return resolve(root_vnode.?, path[1..], flags);
    }

    std.debug.assert(!std.fs.path.isAbsolute(path));

    var next = cwd.?;
    var iter = std.mem.split(u8, path, std.fs.path.sep_str);

    if (path.len > 0) {
        while (iter.next()) |component| {
            if (std.mem.eql(u8, component, ".")) {
                continue;
            } else if (std.mem.eql(u8, component, "..")) {
                next = next.parent orelse next;
            } else {
                next = next.open(component, 0) catch |err| blk: {
                    switch (err) {
                        error.FileNotFound => {
                            const fs = next.getEffectiveFs();

                            if (flags & std.os.linux.O.CREAT != 0) {
                                if (iter.rest().len > 0) {
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
        }
    }

    return next;
}
