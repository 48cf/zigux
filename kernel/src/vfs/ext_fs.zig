const logger = std.log.scoped(.extfs);

const root = @import("root");
const std = @import("std");

const utils = @import("../utils.zig");
const vfs = @import("../vfs.zig");

const FileSystemState = enum(u16) {
    Clean = 1,
    HasErrors = 2,
};

const ErrorHandlingMethod = enum(u16) {
    Ignore = 1,
    RemountAsRo = 2,
    Panic = 3,
};

const BlockGroupDesc = extern struct {
    block_bitmap_addr: u32,
    inode_bitmap_addr: u32,
    inode_table_block: u32,
    unallocated_block_count: u16,
    unallocated_inode_count: u16,
    directory_count: u16,
    padding: [32 - 18]u8,
};

const Inode = extern struct {
    type_and_permissions: u16,
    user_id: u16,
    size_low: u32,
    access_time: u32,
    creation_time: u32,
    modification_time: u32,
    deletion_time: u32,
    group_id: u16,
    hard_link_count: u16,
    disk_sector_count: u32,
    flags: u32,
    os_specific_1: u32,
    direct_block_pointer: [12]u32,
    singly_indirect_block_pointer: u32,
    doubly_indirect_block_pointer: u32,
    triply_indirect_block_pointer: u32,
    generation: u32,
    extended_attribute_block: u32,
    size_high_or_dir_acl: u32,
    fragment_block_address: u32,
    os_specific_2: [12]u8,

    fn kind(self: *const @This()) vfs.VNodeKind {
        return switch (@truncate(u4, self.type_and_permissions >> 12)) {
            0x1 => .Fifo,
            0x2 => .CharaterDevice,
            0x4 => .Directory,
            0x6 => .BlockDevice,
            0x8 => .File,
            0xA => .Symlink,
            0xC => .Socket,
            else => |inode_kind| std.debug.panic("Unknown inode file type: 0x{X}", .{inode_kind}),
        };
    }

    fn size(self: *const @This()) u64 {
        return switch (self.kind()) {
            .File => @as(u64, self.size_high_or_dir_acl) << 32 | @as(u64, self.size_low),
            else => return self.size_low,
        };
    }
};

const SuperBlock = extern struct {
    inode_count: u32,
    block_count: u32,
    superuser_reserved_block_count: u32,
    unallocated_block_count: u32,
    unallocated_inode_count: u32,
    superblock_starting_block: u32,
    log2_block_size: u32,
    log2_fragment_size: u32,
    blocks_per_group: u32,
    fragments_per_group: u32,
    inodes_per_group: u32,
    last_mount_time: u32,
    last_write_time: u32,
    times_mounted_since_fsck: u16,
    mounts_before_fsck_required: u16,
    signature: u16,
    state: FileSystemState,
    error_handling: ErrorHandlingMethod,
    version_minor: u16,
    last_fsck_time: u32,
    forced_fsck_interval: u32,
    created_from_os_id: u32,
    version_major: u32,
    superuser_uid: u16,
    superuser_gid: u16,
    // Extended superblock
    first_inode: u32,
    inode_size: u16,
    block_group: u16,
    optional_features: u32,
    required_features: u32,
    write_required_features: u32,
    fs_id: [16]u8,
    volume_name: [16:0]u8,
    last_mount_path: [64:0]u8,
    compression_algorithm: u32,
    file_blocks_to_preallocate: u8,
    directory_blocks_to_preallocate: u8,
    unused_1: u16,
    journal_id: [16]u8,
    journal_inode: u32,
    journal_device: u32,
    orphan_inode_list_head: u32,
    unused_2: [1024 - 236]u8,

    fn inodeSize(self: *const @This()) u64 {
        return if (self.version_major == 0) 128 else self.inode_size;
    }

    fn blockSize(self: *const @This()) u64 {
        return @as(u64, 1024) << @intCast(u6, self.log2_block_size);
    }

    fn blockGroups(self: *const @This()) u64 {
        return utils.divRoundUp(u64, self.block_count, self.blocks_per_group);
    }

    fn blockGroupOffset(self: *const @This(), index: u64) u64 {
        std.debug.assert(index < self.blockGroups());

        const block_group_desc_offset = utils.alignUp(u64, 2048, self.blockSize());
        return block_group_desc_offset + @sizeOf(BlockGroupDesc) * index;
    }

    fn inodeToBlockGroup(self: *const @This(), inode: u64) u64 {
        return (inode - 1) / self.inodes_per_group;
    }

    fn inodeIndex(self: *const @This(), inode: u64) u64 {
        return (inode - 1) % self.inodes_per_group;
    }

    fn blockToOffset(self: *const @This(), index: u64) u64 {
        return self.blockSize() * index;
    }
};

const ExtFS = struct {
    filesystem: vfs.FileSystem,
    block_device: *vfs.VNode,
    superblock: SuperBlock,

    fn readBlockGroupDesc(self: *const @This(), index: u64) !BlockGroupDesc {
        var block_group: BlockGroupDesc = undefined;
        try self.block_device.readAll(std.mem.asBytes(&block_group), self.superblock.blockGroupOffset(index), 0);

        return block_group;
    }

    fn resolveInodeBlock(self: *const @This(), inode: *const Inode, block: u64) u64 {
        std.debug.assert(block < utils.divRoundUp(u64, inode.size(), self.superblock.blockSize()));

        if (block < 12) {
            return inode.direct_block_pointer[block];
        } else @panic("TOO MANY BLOCKS AAAAAAAAAAAA");
    }

    fn readInode(self: *const @This(), index: u64) !Inode {
        var inode: Inode = undefined;

        const block_group_index = self.superblock.inodeToBlockGroup(index);
        const block_group = try self.readBlockGroupDesc(block_group_index);

        const inode_index = self.superblock.inodeIndex(index);
        const inode_offset = self.superblock.blockToOffset(block_group.inode_table_block) + inode_index * self.superblock.inodeSize();

        try self.block_device.readAll(std.mem.asBytes(&inode), inode_offset, 0);
        return inode;
    }
};

pub fn init(block: *vfs.VNode) !void {
    var ext_fs: ExtFS = .{
        .filesystem = .{
            .vtable = &.{
                .create_file = null,
                .create_dir = null,
                .create_symlink = null,
                .allocate_inode = undefined,
            },
            .case_sensitive = true,
            .name = "ExtFS",
        },
        .block_device = block,
        .superblock = undefined,
    };

    try block.readAll(std.mem.asBytes(&ext_fs.superblock), 1024, 0);

    if (ext_fs.superblock.signature == 0xef53) {
        var root_inode = try ext_fs.readInode(2);
        var root_block = try root.allocator.alloc(u8, ext_fs.superblock.blockSize());
        defer root.allocator.free(root_block);

        var root_block_index = ext_fs.resolveInodeBlock(&root_inode, 0);
        var root_block_offset = ext_fs.superblock.blockToOffset(root_block_index);
        try block.readAll(root_block, root_block_offset, 0);

        logger.debug("{}", .{root_inode});
        logger.debug("{}", .{std.fmt.fmtSliceHexUpper(root_block)});
    }
}
