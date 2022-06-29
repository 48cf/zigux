const logger = std.log.scoped(.phys);

const std = @import("std");
const utils = @import("../utils/utils.zig");

const Bitmap = struct {
    data: []u8,
    base: u64,
    total_pages: u64,
    used_pages: u64,

    inline fn pageToBit(self: *const @This(), addr: u64) u64 {
        return (addr - self.base) / 0x1000;
    }

    inline fn testPage(self: *const @This(), addr: u64) bool {
        const bit = self.pageToBit(addr);
        return self.data[bit / 8] & (@as(u8, 1) << @intCast(u3, bit % 8)) != 0;
    }

    inline fn setPage(self: *@This(), addr: u64) void {
        const bit = self.pageToBit(addr);
        self.data[bit / 8] |= @as(u8, 1) << @intCast(u3, bit % 8);
        self.used_pages += 1;
    }

    inline fn clearPage(self: *@This(), addr: u64) void {
        const bit = self.pageToBit(addr);
        self.data[bit / 8] &= ~(@as(u8, 1) << @intCast(u3, bit % 8));
        self.used_pages -= 1;
    }
};

var bitmap: Bitmap = undefined;

pub fn initialize(
    dram_base: u64,
    dram_size: u64,
    image_base: u64,
    image_size: u64,
) void {
    const data = @intToPtr([*]u8, dram_base)[0..dram_size];
    const data_pages = utils.divRoundUp(u64, dram_size, 0x1000);
    const bitmap_size = utils.divRoundUp(u64, data_pages, 8);
    const bitmap_pages = utils.divRoundUp(u64, bitmap_size, 0x1000);

    bitmap = .{
        .data = data[data.len - bitmap_size ..],
        .base = dram_base,
        .total_pages = data_pages,
        .used_pages = 0,
    };

    std.mem.set(u8, bitmap.data, 0);

    for (utils.range(bitmap_pages)) |_, i| {
        bitmap.setPage(dram_base + dram_size - (i + 1) * 0x1000);
    }

    const reserved_size = image_base + image_size - dram_base;
    const reserved_pages = utils.divRoundUp(u64, reserved_size, 0x1000);

    for (utils.range(reserved_pages)) |_, i| {
        bitmap.setPage(dram_base + i * 0x1000);
    }

    logger.info("Total available memory: {}MiB", .{bitmap.total_pages * 0x1000 / 1024 / 1024});
    logger.info("Reserved memory: {}MiB", .{bitmap.used_pages * 0x1000 / 1024 / 1024});
}

pub fn allocate(pages: u64) ?u64 {
    _ = pages;
    return null;
}

pub fn free(addr: u64, pages: u64) void {
    _ = addr;
    _ = pages;
}
