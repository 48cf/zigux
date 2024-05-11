const logger = std.log.scoped(.devfs);

const root = @import("root");
const std = @import("std");

const abi = @import("../abi.zig");
const debug = @import("../debug.zig");
const vfs = @import("../vfs.zig");
const utils = @import("../utils.zig");
const per_cpu = @import("../per_cpu.zig");
const input = @import("../drivers/input.zig");
const ext_fs = @import("ext_fs.zig");
const ram_fs = @import("ram_fs.zig");

const tty_vtable: vfs.VNodeVTable = .{
    .read = TtyVNode.read,
    .write = TtyVNode.write,
    .ioctl = TtyVNode.ioctl,
    .stat = TtyVNode.stat,
};

var disk_number: usize = 1;

const BlockDeviceError = error{
    InputOutput,
};

const BlockDeviceVTable = struct {
    read_block: fn (self: *BlockDevice, block: usize, buffer: []u8) BlockDeviceError!void,
    write_block: fn (self: *BlockDevice, block: usize, buffer: []const u8) BlockDeviceError!void,
};

const BlockDevice = struct {
    vnode: vfs.VNode,
    vtable: *const BlockDeviceVTable,
    sector_size: usize,
    sector_count: usize,
    name: [24]u8 = undefined,

    const vnode_vtable: vfs.VNodeVTable = .{
        .read = BlockDevice.read,
        .write = BlockDevice.write,
        .stat = BlockDevice.stat,
    };

    fn iterateSectors(
        self: *@This(),
        buffer_in: anytype,
        disk_offset_in: usize,
        small_callback: anytype,
        large_callback: anytype,
    ) !void {
        if (buffer_in.len == 0) {
            return;
        }

        var first_sector = utils.alignDown(usize, disk_offset_in, self.sector_size) / self.sector_size;
        const last_sector = utils.alignDown(usize, disk_offset_in + buffer_in.len - 1, self.sector_size) / self.sector_size;

        if (first_sector == last_sector) {
            return small_callback(self, buffer_in, first_sector, disk_offset_in % self.sector_size);
        }

        var disk_offset = disk_offset_in;
        var buffer = buffer_in;

        if (!utils.isAligned(usize, disk_offset, self.sector_size)) {
            const step = utils.alignUp(usize, disk_offset, self.sector_size) - disk_offset;
            try small_callback(self, buffer[0..step], first_sector, self.sector_size - step);
            buffer = buffer[step..];
            disk_offset += step;
            first_sector += 1;
        }

        while (buffer.len >= self.sector_size) {
            try large_callback(self, buffer[0..self.sector_size], first_sector);
            buffer = buffer[self.sector_size..];
            disk_offset += self.sector_size;
            first_sector += 1;
        }

        if (buffer.len == 0) {
            return;
        }

        try small_callback(self, buffer, first_sector, 0);
    }

    fn doLargeRead(self: *BlockDevice, buffer: []u8, sector: usize) !void {
        return self.vtable.read_block(self, sector, buffer);
    }

    fn doLargeWrite(self: *BlockDevice, buffer: []const u8, sector: usize) !void {
        return self.vtable.write_block(self, sector, buffer);
    }

    fn doSmallRead(self: *BlockDevice, buffer: []u8, sector: usize, offset: usize) !void {
        var temp_buffer: [512]u8 = undefined;
        try self.vtable.read_block(self, sector, &temp_buffer);
        @memcpy(buffer, temp_buffer[offset..][0..buffer.len]);
    }

    fn doSmallWrite(self: *BlockDevice, buffer: []const u8, sector: usize, offset: usize) !void {
        var temp_buffer: [512]u8 = undefined;
        try self.vtable.read_block(self, sector, &temp_buffer);
        @memcpy(temp_buffer[offset..][0..buffer.len], buffer);
        try self.vtable.write_block(self, sector, &temp_buffer);
    }

    fn read(vnode: *vfs.VNode, buffer: []u8, offset: usize, _: usize) vfs.ReadError!usize {
        const self: *@This() = @fieldParentPtr("vnode", vnode);
        const max_read = @min(buffer.len, self.sector_count * self.sector_size - offset);
        try self.iterateSectors(buffer[0..max_read], offset, doSmallRead, doLargeRead);
        return max_read;
    }

    fn write(vnode: *vfs.VNode, buffer: []const u8, offset: usize, _: usize) vfs.WriteError!usize {
        const self: *@This() = @fieldParentPtr("vnode", vnode);
        const max_write = @min(buffer.len, self.sector_count * self.sector_size - offset);
        try self.iterateSectors(buffer[0..max_write], offset, doSmallWrite, doLargeWrite);
        return max_write;
    }

    fn stat(vnode: *vfs.VNode, buffer: *abi.stat) vfs.StatError!void {
        const self: *@This() = @fieldParentPtr("vnode", vnode);
        buffer.* = std.mem.zeroes(abi.stat);
        buffer.st_mode = 0o777 | abi.S_IFBLK;
        buffer.st_size = @intCast(self.sector_count * self.sector_size);
        buffer.st_blksize = @intCast(self.sector_size);
        buffer.st_blocks = @intCast(self.sector_count);
    }
};

fn BlockDeviceWrapper(comptime T: type) type {
    return struct {
        block: BlockDevice,
        device: T,

        const block_vtable: BlockDeviceVTable = .{
            .read_block = @This().readBlock,
            .write_block = @This().writeBlock,
        };

        fn readBlock(block_dev: *BlockDevice, block: usize, buffer: []u8) BlockDeviceError!void {
            const self: *@This() = @fieldParentPtr("block", block_dev);
            try self.device.readBlock(block, buffer);
        }

        fn writeBlock(block_dev: *BlockDevice, block: usize, buffer: []const u8) BlockDeviceError!void {
            const self: *@This() = @fieldParentPtr("block", block_dev);
            try self.device.writeBlock(block, buffer);
        }
    };
}

const PartitionBlockDeviceWrapper = struct {
    block: BlockDevice,
    parent: *BlockDevice,
    start_block: usize,
    end_block: usize,

    const block_vtable: BlockDeviceVTable = .{
        .read_block = @This().readBlock,
        .write_block = @This().writeBlock,
    };

    fn readBlock(block_dev: *BlockDevice, block: usize, buffer: []u8) BlockDeviceError!void {
        const self: *@This() = @fieldParentPtr("block", block_dev);
        std.debug.assert(self.start_block + block < self.end_block);
        return self.parent.vtable.read_block(self.parent, self.start_block + block, buffer);
    }

    fn writeBlock(block_dev: *BlockDevice, block: usize, buffer: []const u8) BlockDeviceError!void {
        const self: *@This() = @fieldParentPtr("block", block_dev);
        std.debug.assert(self.start_block + block < self.end_block);
        return self.parent.vtable.write_block(self.parent, self.start_block + block, buffer);
    }
};

var tty_buffer: @import("../containers/ring_buffer.zig").RingBuffer(u8, 16) = .{};

const TtyVNode = struct {
    vnode: vfs.VNode,
    state: abi.termios = std.mem.zeroInit(abi.termios, .{
        .c_lflag = abi.ECHO | abi.ICANON,
    }),

    fn read(_: *vfs.VNode, _: []u8, _: usize, _: usize) vfs.ReadError!usize {
        return 0;
    }

    fn write(_: *vfs.VNode, buffer: []const u8, _: usize, _: usize) vfs.WriteError!usize {
        return buffer.len;
    }

    fn ioctl(vnode: *vfs.VNode, request: u64, arg: u64) vfs.IoctlError!u64 {
        const self: *@This() = @fieldParentPtr("vnode", vnode);
        const process = per_cpu.get().currentProcess().?;
        switch (request) {
            abi.TCGETS => {
                const result = process.validatePointer(abi.termios, arg) catch return .{ .err = abi.EINVAL };
                result.* = self.state;
                return 0;
            },
            abi.TCSETSW, abi.TCSETSF, abi.TCSETS => {
                const result = process.validatePointer(abi.termios, arg) catch return .{ .err = abi.EINVAL };
                self.state = result.*;
                return 0;
            },
            else => {
                logger.warn("Unhandled TTY IO control request 0x{X}", .{request});
                return error.InvalidArgument;
            },
        }
    }

    fn stat(_: *vfs.VNode, buffer: *abi.stat) vfs.StatError!void {
        buffer.* = std.mem.zeroes(abi.stat);
        buffer.st_mode = 0o777 | abi.S_IFCHR;
    }
};

pub fn init(name: []const u8, parent: ?*vfs.VNode) !*vfs.VNode {
    const ramfs = try ram_fs.init(name, parent);
    ramfs.filesystem.name = "devfs";

    const tty = try root.allocator.create(TtyVNode);
    tty.* = .{
        .vnode = .{
            .vtable = &tty_vtable,
            .filesystem = ramfs.filesystem,
            .kind = .character_device,
            .name = "tty",
        },
    };
    try ramfs.insert(&tty.vnode);

    return ramfs;
}

pub fn addDiskBlockDevice(name: []const u8, device: anytype) !void {
    const WrappedDevice = BlockDeviceWrapper(@TypeOf(device));

    const dev = try vfs.resolve(null, "/dev", 0);
    const node = try root.allocator.create(WrappedDevice);
    const disk_id = @atomicRmw(usize, &disk_number, .Add, 1, .acq_rel);
    node.* = .{
        .block = .{
            .vnode = undefined,
            .vtable = &WrappedDevice.block_vtable,
            .sector_size = device.getSectorSize(),
            .sector_count = device.getSectorCount(),
        },
        .device = device,
    };
    node.block.vnode = .{
        .vtable = &BlockDevice.vnode_vtable,
        .filesystem = dev.filesystem,
        .name = try std.fmt.bufPrint(&node.block.name, "{s}{d}", .{ name, disk_id }),
    };
    try dev.insert(&node.block.vnode);
    try probePartitions(&node.block, node.block.sector_size);
}

const MbrHeader = extern struct {
    reserved0: [510]u8,
    magic: u16,
};

const GptHeader = extern struct {
    signature: [8]u8,
    revision: u32,
    header_size: u32,
    header_crc32: u32,
    reserved14: [4]u8,
    current_lba: u64,
    backup_lba: u64,
    first_usable_lba: u64,
    last_usable_lba: u64,
    disk_guid: std.os.uefi.Guid,
    partition_entry_lba: u64,
    num_partition_entries: u32,
    size_of_partition_entry: u32,
    partition_entry_array_crc32: u32,
};

const GptPartitionEntry = extern struct {
    partition_type_guid: std.os.uefi.Guid,
    unique_partition_guid: std.os.uefi.Guid,
    starting_lba: u64,
    ending_lba: u64,
    attributes: u64,
    name: [36]u16,
};

fn probePartitions(device: *BlockDevice, sector_size: usize) !void {
    var buffer = try root.allocator.alloc(u8, sector_size * 2);
    _ = try device.vnode.read(buffer, 0, 0);
    const dev = try vfs.resolve(null, "/dev", 0);
    const mbr_header = @as(*align(1) const MbrHeader, @ptrCast(buffer));
    const gpt_header = @as(*align(1) const GptHeader, @ptrCast(buffer[512..]));
    if (std.mem.eql(u8, &gpt_header.signature, "EFI PART") and gpt_header.revision == 0x10000) {
        var entry: GptPartitionEntry = undefined;

        for (0..10) |i| {
            _ = try device.vnode.read(
                std.mem.asBytes(&entry),
                gpt_header.partition_entry_lba * sector_size + i * gpt_header.size_of_partition_entry,
                0,
            );

            if (entry.starting_lba == 0) {
                break;
            }

            const part_node = try root.allocator.create(PartitionBlockDeviceWrapper);
            part_node.* = .{
                .block = .{
                    .vnode = undefined,
                    .vtable = &PartitionBlockDeviceWrapper.block_vtable,
                    .sector_size = device.sector_size,
                    .sector_count = entry.ending_lba - entry.starting_lba + 1,
                },
                .parent = device,
                .start_block = entry.starting_lba,
                .end_block = entry.ending_lba + 1,
            };
            part_node.block.vnode = .{
                .vtable = &BlockDevice.vnode_vtable,
                .filesystem = dev.filesystem,
                .name = try std.fmt.bufPrint(&part_node.block.name, "{s}p{d}", .{ device.vnode.name, i + 1 }),
            };
            try dev.insert(&part_node.block.vnode);

            logger.debug(
                "{}: Added partition with start LBA of {} and end LBA of {}",
                .{ part_node.block.vnode.getFullPath(), entry.starting_lba, entry.ending_lba },
            );

            try ext_fs.init(&part_node.block.vnode);
        }
    } else if (mbr_header.magic == 0xAA55) {
        logger.debug("{}: MBR partition table detected", .{device.vnode.getFullPath()});
    } else {
        logger.debug("{}: No partition table detected", .{device.vnode.getFullPath()});
    }
}
