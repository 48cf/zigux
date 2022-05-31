const logger = std.log.scoped(.phys);

const std = @import("std");
const arch = @import("arch.zig");
const limine = @import("limine.zig");
const mutex = @import("mutex.zig");
const utils = @import("utils.zig");
const virt = @import("virt.zig");

const Bitmap = struct {
    data: []u8,

    pub fn init(data: []u8) Bitmap {
        return Bitmap{ .data = data };
    }

    pub fn testBit(self: *const Bitmap, bit: usize) bool {
        return self.data[bit / 8] & @as(u8, 1) << @intCast(u3, bit % 8) != 0;
    }

    pub fn setBit(self: *Bitmap, bit: usize) void {
        self.data[bit / 8] |= @as(u8, 1) << @intCast(u3, bit % 8);
    }

    pub fn clearBit(self: *Bitmap, bit: usize) void {
        self.data[bit / 8] &= ~(@as(u8, 1) << @intCast(u3, bit % 8));
    }
};

var lock: mutex.AtomicMutex = .{};
var bitmap: Bitmap = undefined;
var highest_phys_addr: u64 = 0;

var total_pages: usize = undefined;
var used_pages: usize = undefined;
var last_allocation: usize = undefined;

pub fn init(memory_map_res: *limine.MemoryMap.Response) !void {
    const entries = memory_map_res.entries[0..memory_map_res.entry_count];

    logger.info("Current system memory map:", .{});

    var bitmap_region: ?*limine.MemoryMap.Entry = null;

    for (entries) |entry| {
        logger.info("  base=0x{X:0>16}, length=0x{X:0>16}, kind={}", .{ entry.base, entry.length, entry.kind });

        if (entry.kind == .Usable and entry.base + entry.length > highest_phys_addr) {
            highest_phys_addr = entry.base + entry.length;
        }
    }

    const bitmap_size = utils.alignUp(u64, highest_phys_addr, std.mem.page_size) / std.mem.page_size / 8 + 1;

    logger.debug("Highest available address: 0x{X:0>16}", .{highest_phys_addr});
    logger.debug("Required bitmap size: {}KiB", .{bitmap_size / 1024});

    for (entries) |entry| {
        if (entry.kind == .Usable and entry.length >= bitmap_size) {
            bitmap_region = entry;
            break;
        }
    }

    if (bitmap_region == null) {
        return error.BitmapTooBig;
    }

    bitmap = blk: {
        const bitmap_data = virt.asHigherHalf([*]u8, bitmap_region.?.base);

        break :blk Bitmap.init(bitmap_data[0..bitmap_size]);
    };

    total_pages = 0;
    used_pages = 0;
    last_allocation = 0;

    std.mem.set(u8, bitmap.data, 0xFF);

    for (entries) |entry| {
        if (entry.kind == .Usable) {
            const base = utils.alignDown(u64, entry.base, std.mem.page_size) / std.mem.page_size;
            const length = utils.alignUp(u64, entry.length, std.mem.page_size) / std.mem.page_size;

            var i: usize = 0;

            while (i < length) : (i += 1) {
                bitmap.clearBit(base + i);
            }

            total_pages += length;
        }
    }

    const bitmap_base = utils.alignDown(u64, bitmap_region.?.base, std.mem.page_size) / std.mem.page_size;
    const bitmap_length = utils.alignUp(u64, bitmap_size, std.mem.page_size) / std.mem.page_size;

    var i: usize = 0;

    while (i < bitmap_length) : (i += 1) {
        bitmap.setBit(bitmap_base + i);
    }

    total_pages -= bitmap_length;

    logger.info("Amount of memory available: {}MiB", .{(total_pages * std.mem.page_size) / 1024 / 1024});
}

pub fn allocate(pages: usize, zero: bool) ?u64 {
    lock.lock();

    defer lock.unlock();

    if (pages > total_pages - used_pages) {
        return null;
    }

    const allocation = allocate_inner(last_allocation, pages) orelse allocate_inner(0, pages);

    if (allocation == null) {
        return null;
    }

    var i: usize = 0;

    while (i < pages) : (i += 1) {
        bitmap.setBit(allocation.? + i);
    }

    last_allocation = allocation.? + pages;
    used_pages += pages;

    const address = allocation.? * std.mem.page_size;

    if (zero) {
        const allocation_data = virt.asHigherHalf([*]u8, address);

        @memset(allocation_data, 0, pages * std.mem.page_size);
    }

    return address;
}

// TODO: Implement free lmao
pub fn free(address: u64, pages: usize) void {
    logger.warn("Physical free is a stub, attempt to free {} pages at 0x{X}", .{ pages, address });
}

pub fn highestAddress() u64 {
    return highest_phys_addr;
}

pub fn freePages() usize {
    return total_pages - used_pages;
}

pub fn totalPages() usize {
    return total_pages;
}

pub fn usedPages() usize {
    return used_pages;
}

fn allocate_inner(start: usize, pages: usize) ?u64 {
    var i: usize = start;

    while (i < bitmap.data.len * 8) : (i += 1) {
        var found = true;
        var j: usize = 0;

        while (found and j < pages) : (j += 1) {
            found = !bitmap.testBit(i + j);
        }

        if (found) {
            return i;
        }
    }

    return null;
}
