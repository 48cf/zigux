const logger = std.log.scoped(.phys);

const std = @import("std");
const arch = @import("arch.zig");
const limine = @import("limine.zig");
const phys = @import("phys.zig");
const utils = @import("utils.zig");

const flags_mask: u64 = 0xFFF0000000000FFF;

pub const Flags = struct {
    pub const None = 0;
    pub const Present = 1 << 0;
    pub const Writable = 1 << 1;
    pub const User = 1 << 2;
    pub const WriteThrough = 1 << 3;
    pub const NoCache = 1 << 4;
    pub const NoExecute = 1 << 63;
};

fn get_page_table(page_table: *PageTable, index: usize, allocate: bool) ?*PageTable {
    var entry = &page_table.entries[index];

    if (entry.getFlags() & Flags.Present != 0) {
        return @intToPtr(*PageTable, hhdm + entry.getAddress());
    } else if (allocate) {
        const new_page_table = phys.allocate(1, true) orelse return null;

        entry.setAddress(new_page_table);
        entry.setFlags(Flags.Present | Flags.Writable | Flags.User);

        return @intToPtr(*PageTable, hhdm + new_page_table);
    }

    return null;
}

fn virt_to_indices(virt: u64) struct { pml4: usize, pml3: usize, pml2: usize, pml1: usize } {
    const pml4 = @as(usize, virt >> 39) & 0x1FF;
    const pml3 = @as(usize, virt >> 30) & 0x1FF;
    const pml2 = @as(usize, virt >> 21) & 0x1FF;
    const pml1 = @as(usize, virt >> 12) & 0x1FF;

    return .{
        .pml4 = pml4,
        .pml3 = pml3,
        .pml2 = pml2,
        .pml1 = pml1,
    };
}

const PageTableEntry = packed struct {
    value: u64,

    pub fn getAddress(self: *const PageTableEntry) u64 {
        return self.value & ~flags_mask;
    }

    pub fn getFlags(self: *const PageTableEntry) u64 {
        return self.value & flags_mask;
    }

    pub fn setAddress(self: *PageTableEntry, address: u64) void {
        self.value = address | self.getFlags();
    }

    pub fn setFlags(self: *PageTableEntry, flags: u64) void {
        self.value = self.getAddress() | flags;
    }
};

const PageTable = packed struct {
    entries: [512]PageTableEntry,

    pub fn translate(self: *PageTable, virt_addr: u64) ?u64 {
        const indices = virt_to_indices(virt_addr);
        const pml3 = get_page_table(self, indices.pml4, false) orelse return error.OutOfMemory;
        const pml2 = get_page_table(pml3, indices.pml3, false) orelse return error.OutOfMemory;
        const pml1 = get_page_table(pml2, indices.pml2, false) orelse return error.OutOfMemory;
        const entry = &pml1.entries[indices.pml1];

        if (entry.getFlags() & Flags.Present != 0) {
            return entry.getAddress();
        } else {
            return null;
        }
    }

    pub fn mapPage(self: *PageTable, virt_addr: u64, phys_addr: u64, flags: u64) !void {
        const indices = virt_to_indices(virt_addr);
        const pml3 = get_page_table(self, indices.pml4, true) orelse return error.OutOfMemory;
        const pml2 = get_page_table(pml3, indices.pml3, true) orelse return error.OutOfMemory;
        const pml1 = get_page_table(pml2, indices.pml2, true) orelse return error.OutOfMemory;
        const entry = &pml1.entries[indices.pml1];

        if (entry.getFlags() & Flags.Present != 0) {
            return error.AlreadyMapped;
        } else {
            entry.setAddress(phys_addr);
            entry.setFlags(flags);
        }
    }

    pub fn remapPage(self: *PageTable, virt_addr: u64, phys_addr: u64, flags: u64) !void {
        const indices = virt_to_indices(virt_addr);
        const pml3 = get_page_table(self, indices.pml4, false) orelse return error.OutOfMemory;
        const pml2 = get_page_table(pml3, indices.pml3, false) orelse return error.OutOfMemory;
        const pml1 = get_page_table(pml2, indices.pml2, false) orelse return error.OutOfMemory;
        const entry = &pml1.entries[indices.pml1];

        if (entry.getFlags() & Flags.Present != 0) {
            entry.setAddress(phys_addr);
            entry.setFlags(flags);

            asm volatile ("invlpg %[page]"
                :
                : [page] "rm" (virt_addr),
            );
        } else {
            return error.NotMapped;
        }
    }

    pub fn unmapPage(self: *PageTable, virt_addr: u64) !void {
        const indices = virt_to_indices(virt_addr);
        const pml3 = get_page_table(self, indices.pml4, false) orelse return error.OutOfMemory;
        const pml2 = get_page_table(pml3, indices.pml3, false) orelse return error.OutOfMemory;
        const pml1 = get_page_table(pml2, indices.pml2, false) orelse return error.OutOfMemory;
        const entry = &pml1.entries[indices.pml1];

        if (entry.getFlags() & Flags.Present != 0) {
            entry.setAddress(0);
            entry.setFlags(.None);

            asm volatile ("invlpg %[page]"
                :
                : [page] "rm" (virt_addr),
            );
        } else {
            return error.NotMapped;
        }
    }

    pub fn map(self: *PageTable, virt_addr: u64, phys_addr: u64, size: usize, flags: u64) !void {
        var i: u64 = 0;

        while (i < size) : (i += std.mem.page_size) {
            try self.mapPage(virt_addr + i, phys_addr + i, flags);
        }
    }

    pub fn remap(self: *PageTable, virt_addr: u64, phys_addr: u64, size: usize, flags: u64) !void {
        var i: u64 = 0;

        while (i < size) : (i += std.mem.page_size) {
            try self.remapPage(virt_addr + i, phys_addr + i, flags);
        }
    }

    pub fn unmap(self: *PageTable, virt_addr: u64, size: usize) !void {
        var i: u64 = 0;

        while (i < size) : (i += std.mem.page_size) {
            try self.unmapPage(virt_addr + i);
        }
    }
};

pub const AddressSpace = struct {
    cr3: u64,
    page_table: *PageTable,
    alloc_base: u64,
};

pub var hhdm: u64 = 0;
pub var kernel_page_table: *PageTable = undefined;
pub var kernel_address_space: ?AddressSpace = null;

fn map_section(
    comptime section_name: []const u8,
    page_table: *PageTable,
    kernel_addr_res: *limine.KernelAddress.Response,
    flags: u64,
) !void {
    const begin = @extern(*u8, .{ .name = section_name ++ "_begin" });
    const end = @extern(*u8, .{ .name = section_name ++ "_end" });

    const virt_base = @ptrToInt(begin);
    const phys_base = virt_base - kernel_addr_res.virtual_base + kernel_addr_res.physical_base;
    const size = utils.align_up(usize, @ptrToInt(end) - @ptrToInt(begin), std.mem.page_size);

    try page_table.map(virt_base, phys_base, size, flags);
}

pub fn init(hhdm_res: *limine.Hhdm.Response, kernel_addr_res: *limine.KernelAddress.Response) !void {
    const page_table = phys.allocate(1, true) orelse return error.OutOfMemory;

    hhdm = hhdm_res.offset;
    kernel_page_table = @intToPtr(*PageTable, hhdm + page_table);
    kernel_address_space = .{
        .cr3 = page_table,
        .page_table = kernel_page_table,
        .alloc_base = 0,
    };

    var i: usize = 256;

    while (i < 512) : (i += 1) {
        _ = get_page_table(kernel_page_table, i, true);
    }

    try kernel_page_table.map(std.mem.page_size, std.mem.page_size, utils.gib(4) - std.mem.page_size, Flags.Present | Flags.Writable);
    try kernel_page_table.map(hhdm, 0, utils.gib(4), Flags.Present | Flags.Writable);

    try map_section("text", kernel_page_table, kernel_addr_res, Flags.Present);
    try map_section("rodata", kernel_page_table, kernel_addr_res, Flags.Present | Flags.NoExecute);
    try map_section("data", kernel_page_table, kernel_addr_res, Flags.Present | Flags.NoExecute | Flags.Writable);

    asm volatile ("mov %[page_table], %%cr3"
        :
        : [page_table] "r" (page_table),
        : "memory"
    );
}

pub fn createAddressSpace() !AddressSpace {
    const page_table_phys = phys.allocate(1, true) orelse return error.OutOfMemory;
    const page_table = @intToPtr(*PageTable, hhdm + page_table_phys);

    var i: usize = 256;

    while (i < 512) : (i += 1) {
        page_table.entries[i] = kernel_page_table.entries[i];
    }

    return AddressSpace{
        .cr3 = page_table_phys,
        .page_table = page_table,
        .alloc_base = 0,
    };
}
