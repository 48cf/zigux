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
        const self = @as(*RamFSFile, @fieldParentPtr("vnode", vnode));

        _ = flags;

        if (offset >= self.data.items.len) {
            return 0;
        }

        const bytes_read = @min(buffer.len, self.data.items.len - offset);

        @memcpy(
            buffer[0..bytes_read],
            self.data.items[offset .. offset + bytes_read],
        );

        return bytes_read;
    }

    fn write(vnode: *vfs.VNode, buffer: []const u8, offset: usize, flags: usize) vfs.WriteError!usize {
        const self = @as(*RamFSFile, @fieldParentPtr("vnode", vnode));

        _ = flags;

        if (buffer.len + offset > self.data.items.len) {
            self.data.resize(root.allocator, buffer.len + offset) catch return error.OutOfMemory;

            // TODO: Zero out the newly allocated content
        }

        @memcpy(self.data.items[offset..], buffer);

        return buffer.len;
    }

    fn stat(vnode: *vfs.VNode, buffer: *abi.stat) vfs.StatError!void {
        const self = @as(*RamFSFile, @fieldParentPtr("vnode", vnode));

        buffer.* = std.mem.zeroes(abi.stat);
        buffer.st_ino = @as(c_ulong, @intCast(vnode.inode));
        buffer.st_mode = 0o777 | abi.S_IFREG;
        buffer.st_size = @as(c_long, @intCast(self.data.items.len));
        buffer.st_blksize = std.mem.page_size;
        buffer.st_blocks = @as(c_long, @intCast(std.mem.alignForward(usize, self.data.items.len, std.mem.page_size) / std.mem.page_size));
    }
};

const RamFSDirectory = struct {
    vnode: vfs.VNode,
    children: std.ArrayListUnmanaged(*vfs.VNode) = .{},

    fn open(vnode: *vfs.VNode, name: []const u8, flags: usize) vfs.OpenError!*vfs.VNode {
        _ = flags;

        const self = @as(*RamFSDirectory, @fieldParentPtr("vnode", vnode));

        for (self.children.items) |child| {
            if (std.mem.eql(u8, child.name.?, name)) {
                return child;
            }
        }

        return error.FileNotFound;
    }

    fn readDir(vnode: *vfs.VNode, buffer: []u8, offset: *usize) vfs.ReadDirError!usize {
        const self = @as(*RamFSDirectory, @fieldParentPtr("vnode", vnode));

        var dir_ent = @as(*abi.dirent, @ptrCast(@alignCast(buffer)));
        var buffer_offset: usize = 0;

        while (offset.* < self.children.items.len) : (offset.* += 1) {
            const child = self.children.items[offset.*];
            const name = child.name.?;
            const real_size = @sizeOf(abi.dirent) - 1024 + name.len + 1;

            if (buffer_offset + real_size > buffer.len) {
                break;
            }

            dir_ent.d_off = 0;
            dir_ent.d_ino = @as(c_ulong, @intCast(vnode.inode));
            dir_ent.d_reclen = @truncate(real_size);
            dir_ent.d_type = switch (child.kind) {
                .File => abi.DT_REG,
                .Directory => abi.DT_DIR,
                .Symlink => abi.DT_LNK,
                .CharacterDevice => abi.DT_CHR,
                .BlockDevice => abi.DT_BLK,
                .Fifo => abi.DT_FIFO,
                .Socket => abi.DT_SOCK,
            };

            @memcpy(dir_ent.d_name[0..name.len], name);

            dir_ent.d_name[name.len] = 0;
            buffer_offset += real_size;
            dir_ent = @as(*abi.dirent, @ptrCast(@alignCast(buffer[buffer_offset..])));
        }

        return buffer_offset;
    }

    fn insert(vnode: *vfs.VNode, child: *vfs.VNode) vfs.InsertError!void {
        const self = @as(*RamFSDirectory, @fieldParentPtr("vnode", vnode));

        for (self.children.items) |it| {
            if (std.mem.eql(u8, it.name.?, child.name.?)) {
                return error.PathAlreadyExists;
            }
        }

        try self.children.append(root.allocator, child);
    }

    fn stat(vnode: *vfs.VNode, buffer: *abi.stat) vfs.StatError!void {
        buffer.* = std.mem.zeroes(abi.stat);
        buffer.st_ino = @as(c_ulong, @intCast(vnode.inode));
        buffer.st_mode = 0o777 | abi.S_IFDIR;
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
                .kind = .File,
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
                .kind = .Directory,
            },
        };

        return &node.vnode;
    }

    fn createSymlink(fs: *vfs.FileSystem, target: []const u8) vfs.OomError!*vfs.VNode {
        const node = try root.allocator.create(vfs.VNode);

        node.* = .{
            .vtable = &.{},
            .filesystem = fs,
            .kind = .Symlink,
            .symlink_target = try root.allocator.dupe(u8, target),
        };

        return node;
    }

    fn allocateInode(self: *vfs.FileSystem) vfs.OomError!u64 {
        const fs = @as(*RamFS, @fieldParentPtr("filesystem", self));

        return @atomicRmw(usize, &fs.inode_counter, .Add, 1, .acq_rel);
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
