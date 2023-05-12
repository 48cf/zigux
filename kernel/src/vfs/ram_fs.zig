const logger = std.log.scoped(.ramfs);

const root = @import("root");
const std = @import("std");

const abi = @import("../abi.zig");
const phys = @import("../phys.zig");
const utils = @import("../utils.zig");
const vfs = @import("../vfs.zig");
const virt = @import("../virt.zig");

const ram_fs_vtable: vfs.FileSystemVTable = .{
    .create_file = RamFS.createFile,
    .create_dir = RamFS.createDir,
    .create_symlink = RamFS.createSymlink,
    .allocate_inode = RamFS.allocateInode,
};

const ram_fs_file_vtable: vfs.VNodeVTable = .{
    .read = RamFSFile.read,
    .write = RamFSFile.write,
    .stat = RamFSFile.stat,
};

const ram_fs_directory_vtable: vfs.VNodeVTable = .{
    .open = RamFSDirectory.open,
    .read_dir = RamFSDirectory.readDir,
    .insert = RamFSDirectory.insert,
    .stat = RamFSDirectory.stat,
};

const RamFSFile = struct {
    vnode: vfs.VNode,
    data: std.ArrayListAlignedUnmanaged(u8, std.mem.page_size) = .{},

    fn read(vnode: *vfs.VNode, buffer: []u8, offset: usize, flags: usize) vfs.ReadError!usize {
        const self = @fieldParentPtr(RamFSFile, "vnode", vnode);

        _ = flags;

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

    fn write(vnode: *vfs.VNode, buffer: []const u8, offset: usize, flags: usize) vfs.WriteError!usize {
        const self = @fieldParentPtr(RamFSFile, "vnode", vnode);

        _ = flags;

        if (buffer.len + offset > self.data.items.len) {
            self.data.resize(root.allocator, buffer.len + offset) catch return error.OutOfMemory;

            // TODO: Zero out the newly allocated content
        }

        std.mem.copy(u8, self.data.items[offset..], buffer);

        return buffer.len;
    }

    fn stat(vnode: *vfs.VNode, buffer: *abi.C.stat) vfs.StatError!void {
        const self = @fieldParentPtr(RamFSFile, "vnode", vnode);

        buffer.* = std.mem.zeroes(abi.C.stat);
        buffer.st_ino = @intCast(c_ulong, vnode.inode);
        buffer.st_mode = 0o777 | abi.C.S_IFREG;
        buffer.st_size = @intCast(c_long, self.data.items.len);
        buffer.st_blksize = std.mem.page_size;
        buffer.st_blocks = @intCast(c_long, std.mem.alignForward(self.data.items.len, std.mem.page_size) / std.mem.page_size);
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

        return error.FileNotFound;
    }

    fn readDir(vnode: *vfs.VNode, buffer: []u8, offset: *usize) vfs.ReadDirError!usize {
        const self = @fieldParentPtr(RamFSDirectory, "vnode", vnode);

        var dir_ent = @ptrCast(*abi.C.dirent, @alignCast(8, buffer));
        var buffer_offset: usize = 0;

        while (offset.* < self.children.items.len) : (offset.* += 1) {
            const child = self.children.items[offset.*];
            const name = child.name.?;
            const real_size = @sizeOf(abi.C.dirent) - 1024 + name.len + 1;

            if (buffer_offset + real_size > buffer.len) {
                break;
            }

            dir_ent.d_off = 0;
            dir_ent.d_ino = @intCast(c_ulong, vnode.inode);
            dir_ent.d_reclen = @truncate(c_ushort, real_size);
            dir_ent.d_type = switch (child.kind) {
                .File => abi.C.DT_REG,
                .Directory => abi.C.DT_DIR,
                .Symlink => abi.C.DT_LNK,
                .CharaterDevice => abi.C.DT_CHR,
                .BlockDevice => abi.C.DT_BLK,
                .Fifo => abi.C.DT_FIFO,
                .Socket => abi.C.DT_SOCK,
            };

            std.mem.copy(u8, dir_ent.d_name[0..name.len], name);

            dir_ent.d_name[name.len] = 0;
            buffer_offset += real_size;
            dir_ent = @ptrCast(*abi.C.dirent, @alignCast(8, buffer[buffer_offset..]));
        }

        return buffer_offset;
    }

    fn insert(vnode: *vfs.VNode, child: *vfs.VNode) vfs.InsertError!void {
        const self = @fieldParentPtr(RamFSDirectory, "vnode", vnode);

        for (self.children.items) |it| {
            if (std.mem.eql(u8, it.name.?, child.name.?)) {
                return error.PathAlreadyExists;
            }
        }

        try self.children.append(root.allocator, child);
    }

    fn stat(vnode: *vfs.VNode, buffer: *abi.C.stat) vfs.StatError!void {
        buffer.* = std.mem.zeroes(abi.C.stat);
        buffer.st_ino = @intCast(c_ulong, vnode.inode);
        buffer.st_mode = 0o777 | abi.C.S_IFDIR;
    }
};

const RamFS = struct {
    filesystem: vfs.FileSystem,
    root: RamFSDirectory,
    inode_counter: u64,

    fn createFile(fs: *vfs.FileSystem) vfs.OomError!*vfs.VNode {
        const node = try root.allocator.create(RamFSFile);

        node.* = .{
            .vnode = .{
                .vtable = &ram_fs_file_vtable,
                .filesystem = fs,
            },
        };

        return &node.vnode;
    }

    fn createDir(fs: *vfs.FileSystem) vfs.OomError!*vfs.VNode {
        const node = try root.allocator.create(RamFSDirectory);

        node.* = .{
            .vnode = .{
                .vtable = &ram_fs_directory_vtable,
                .filesystem = fs,
            },
        };

        return &node.vnode;
    }

    fn createSymlink(fs: *vfs.FileSystem, target: []const u8) vfs.OomError!*vfs.VNode {
        const node = try root.allocator.create(vfs.VNode);

        node.* = .{
            .vtable = &.{},
            .filesystem = fs,
            .symlink_target = try root.allocator.dupe(u8, target),
        };

        return node;
    }

    fn allocateInode(self: *vfs.FileSystem) vfs.OomError!u64 {
        const fs = @fieldParentPtr(RamFS, "filesystem", self);

        return @atomicRmw(usize, &fs.inode_counter, .Add, 1, .AcqRel);
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
        .inode_counter = 0,
    };

    return &ramfs.root.vnode;
}
