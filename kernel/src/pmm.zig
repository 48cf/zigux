const log = std.log.scoped(.pmm);

const std = @import("std");
const limine = @import("limine");

pub export var memmap_req: limine.MemoryMapRequest = .{};
pub export var hhdm_req: limine.HhdmRequest = .{};

const max_order = 18;
const Header = extern struct {
    next: ?*Header,
};

var hhdm_offset: u64 = 0;
var low_buddies = std.mem.zeroes([max_order + 1]?*Header);
var high_buddies = std.mem.zeroes([max_order + 1]?*Header);

inline fn order_to_size(order: usize) usize {
    return @as(usize, 0x1000) << @intCast(order);
}

inline fn size_to_order(size: usize) usize {
    return @min(std.math.log2_int(usize, size) - 12, max_order);
}

inline fn free_into(buddies: []?*Header, base: u64, order: usize) void {
    const header: *Header = @ptrFromInt(base + hhdm_offset);
    header.next = buddies[order];
    buddies[order] = header;
}

fn allocate_from(buddies: []?*Header, order: usize) ?u64 {
    if (buddies[order]) |buddy| {
        buddies[order] = buddy.next;
        return @intFromPtr(buddy) - hhdm_offset;
    }

    if (order < max_order) {
        const block = allocate_from(buddies, order + 1) orelse return null;
        free_into(buddies, block + order_to_size(order), order);
        return block;
    }

    return null;
}

pub fn init() void {
    const memmap_res = memmap_req.response orelse @panic("Cannot continue without memory map");
    const hhdm_res = hhdm_req.response orelse @panic("Cannot continue without HHDM");

    hhdm_offset = hhdm_res.offset;

    for (memmap_res.entries()) |entry| {
        log.info("[0x{x:0>16}-0x{x:0>16}] {s}", .{ entry.base, entry.base + entry.length, @tagName(entry.kind) });

        if (entry.kind != .usable) {
            continue;
        }

        var offset: usize = 0;

        while (entry.length - offset >= 0x1000) {
            const order = size_to_order(entry.length - offset);
            const offset_base = entry.base + offset;

            if (offset_base < 0x100000000) {
                log.debug(
                    "Freeing [0x{x:0>16}-0x{x:0>16}] into low_buddy#{d}",
                    .{ offset_base, offset_base + order_to_size(order), order },
                );

                free_into(&low_buddies, offset_base, order);
            } else {
                log.debug(
                    "Freeing [0x{x:0>16}-0x{x:0>16}] into high_buddy#{d}",
                    .{ offset_base, offset_base + order_to_size(order), order },
                );

                free_into(&high_buddies, offset_base, order);
            }

            offset += order_to_size(order);
        }
    }
}

pub inline fn allocate_low(pages: usize) ?u64 {
    const order = std.math.log2_int_ceil(usize, pages);
    return allocate_from(&low_buddies, order);
}

pub inline fn allocate(pages: usize) ?u64 {
    const order = std.math.log2_int_ceil(usize, pages);
    return allocate_from(&high_buddies, order) orelse return allocate_low(pages);
}

pub inline fn free(base: u64, pages: usize) void {
    const order = size_to_order(pages * 0x1000);

    if (base < 0x100000000) {
        free_into(&low_buddies, base, order);
    } else {
        free_into(&high_buddies, base, order);
    }
}
