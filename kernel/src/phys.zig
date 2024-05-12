const logger = std.log.scoped(.phys);

const std = @import("std");
const limine = @import("limine");

const arch = @import("arch.zig");
const lock = @import("lock.zig");
const utils = @import("utils.zig");
const virt = @import("virt.zig");

const PageList = std.SinglyLinkedList(void);
const RegionQueue = std.TailQueue(void);

pub const PageUsage = enum(u4) {
    invalid,
    pfn_database,
    free,
    conventional,
    dma,
    stack,
    page_table,
    max = 0b1111,
};

pub const Page = struct {
    node: PageList.Node = .{ .data = {} },
    info: packed struct(u64) {
        pfn: u52,
        usage: PageUsage,
        is_dirty: u1,
        reserved: u7,
    },
};

const PhysicalRegion = struct {
    node: RegionQueue.Node = .{ .data = {} },
    base_address: u64,
    page_count: usize,

    fn pages(self: *@This()) []Page {
        const ptr: [*]Page = @ptrFromInt(@intFromPtr(self) + @sizeOf(PhysicalRegion));
        return ptr[0..self.page_count];
    }
};

var pmm_lock: lock.Spinlock = .{};
var free_pages: PageList = .{};
var regions: RegionQueue = .{};

pub fn init(memory_map_res: *limine.MemoryMapResponse) !void {
    logger.info("Current system memory map:", .{});

    for (memory_map_res.entries()) |entry| {
        if (entry.kind != .usable) {
            continue;
        }

        const page_count = std.math.divCeil(usize, entry.length, std.mem.page_size) catch unreachable;
        const reserved = std.mem.alignForward(usize, @sizeOf(PhysicalRegion) + @sizeOf(Page) * page_count, std.mem.page_size);
        const aligned_length = page_count * std.mem.page_size;

        if (aligned_length - reserved < std.mem.page_size * 8) {
            logger.debug(
                "Skipping usable physical region 0x{X}-0x{X}, too small to track",
                .{ entry.base, entry.base + aligned_length },
            );
            continue;
        }

        const region = virt.asHigherHalf(*PhysicalRegion, entry.base);
        region.* = .{ .base_address = entry.base, .page_count = page_count };

        logger.info(
            "Tracking physical region 0x{X}-0x{X}, {d} KiB, {d} pages, {d} KiB for PFDB",
            .{ region.base_address, region.base_address + aligned_length, aligned_length / 1024, region.page_count, reserved / 1024 },
        );

        const pages = region.pages();
        const reserved_page_count = reserved / std.mem.page_size;

        for (pages, 0..) |*page, i| {
            page.* = Page{
                .info = .{
                    .pfn = @truncate((region.base_address + i * std.mem.page_size) >> 12),
                    .usage = if (i < reserved_page_count) .pfn_database else .free,
                    .is_dirty = @intFromBool(i >= reserved_page_count),
                    .reserved = 0,
                },
            };

            if (i >= reserved_page_count) {
                free_pages.prepend(&page.node);
            }
        }

        regions.append(&region.node);
    }
}

pub fn allocate(pages: usize, usage: PageUsage) ?u64 {
    std.debug.assert(pages == 1);
    std.debug.assert(@intFromEnum(usage) >= @intFromEnum(PageUsage.conventional));

    const page: *Page = blk: {
        pmm_lock.lock();
        defer pmm_lock.unlock();
        break :blk @fieldParentPtr("node", free_pages.popFirst() orelse return null);
    };

    std.debug.assert(page.info.usage == .free);
    page.info.usage = usage;

    const page_phys = page.info.pfn << 12;
    if (page.info.is_dirty == 1) {
        const page_virt = virt.asHigherHalf(*[std.mem.page_size]u8, page_phys);
        @memset(page_virt, 0);
    }

    return page_phys;
}

pub fn free(address: u64, pages: usize) void {
    _ = address;
    _ = pages;
}
