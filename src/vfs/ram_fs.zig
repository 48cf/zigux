const logger = std.log.scoped(.ramfs);

const root = @import("root");
const std = @import("std");

const vfs = @import("../vfs.zig");

const ram_fs_vtable: vfs.FileSystemVTable = .{
    .create_file = RamFS.createFile,
    .create_dir = RamFS.createDir,
};

const ram_fs_file_vtable: vfs.VNodeVTable = .{
    .open = null,
    .read = RamFSFile.read,
    .write = RamFSFile.write,
    .insert = null,
};

const ram_fs_directory_vtable: vfs.VNodeVTable = .{
    .open = RamFSDirectory.open,
    .read = null,
    .write = null,
    .insert = RamFSDirectory.insert,
};

const RamFSFile = struct {
    vnode: vfs.VNode,
    data: std.ArrayListAlignedUnmanaged(u8, std.mem.page_size) = .{},

    fn read(vnode: *vfs.VNode, buffer: []u8, offset: usize) vfs.ReadWriteError!usize {
        const self = @fieldParentPtr(RamFSFile, "vnode", vnode);

        if (offset >= self.data.items.len) {
            return 0;
        }

        const bytes_read = std.math.min(buffer.len, self.data.items.len - offset);

        std.mem.copy(
            u8,
            buffer[0..bytes_read],
            self.data.items[offset .. offset + bytes_read],
        );

        return bytes_read;
    }

    fn write(vnode: *vfs.VNode, buffer: []const u8, offset: usize) vfs.ReadWriteError!usize {
        const self = @fieldParentPtr(RamFSFile, "vnode", vnode);

        if (buffer.len + offset > self.data.items.len) {
            try self.data.resize(root.allocator, buffer.len + offset);
        }

        std.mem.copy(u8, self.data.items, buffer);

        return buffer.len;
    }
};

const RamFSDirectory = struct {
    vnode: vfs.VNode,
    children: std.ArrayListUnmanaged(*vfs.VNode) = .{},

    fn open(vnode: *vfs.VNode, name: []const u8, flags: usize) vfs.OpenError!*vfs.VNode {
        _ = flags;

        const self = @fieldParentPtr(RamFSDirectory, "vnode", vnode);

        for (self.children.items) |child| {
            if (std.mem.eql(u8, child.name.?, name)) {
                return child;
            }
        }

        return error.NotFound;
    }

    fn insert(vnode: *vfs.VNode, child: *vfs.VNode) !void {
        const self = @fieldParentPtr(RamFSDirectory, "vnode", vnode);

        try self.children.append(root.allocator, child);
    }
};

const RamFS = struct {
    filesystem: vfs.FileSystem,
    root: RamFSDirectory,

    fn createFile(fs: *vfs.FileSystem) error{OutOfMemory}!*vfs.VNode {
        const node = try root.allocator.create(RamFSFile);

        node.* = .{
            .vnode = .{
                .vtable = &ram_fs_file_vtable,
                .filesystem = fs,
            },
        };

        return &node.vnode;
    }

    fn createDir(fs: *vfs.FileSystem) error{OutOfMemory}!*vfs.VNode {
        const node = try root.allocator.create(RamFSDirectory);

        node.* = .{
            .vnode = .{
                .vtable = &ram_fs_directory_vtable,
                .filesystem = fs,
            },
        };

        return &node.vnode;
    }
};

pub fn init(name: []const u8, parent: ?*vfs.VNode) !*vfs.VNode {
    const ramfs = try root.allocator.create(RamFS);

    ramfs.* = .{
        .filesystem = .{
            .vtable = &ram_fs_vtable,
            .case_sensitive = true,
            .name = "RamFS",
        },
        .root = .{
            .vnode = .{
                .vtable = &ram_fs_directory_vtable,
                .filesystem = &ramfs.filesystem,
                .kind = .Directory,
                .name = name,
                .parent = parent,
            },
        },
    };

    return &ramfs.root.vnode;
}
