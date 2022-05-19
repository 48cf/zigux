const logger = std.log.scoped(.phys);

const std = @import("std");
const arch = @import("arch.zig");
const limine = @import("limine.zig");
const phys = @import("phys.zig");
const utils = @import("utils.zig");
const vfs = @import("vfs.zig");

const IrqSpinlock = @import("irq_lock.zig").IrqSpinlock;

const flags_mask: u64 = 0xFFF0_0000_0000_0FFF;
const alloc_base: u64 = 0x7000_0000_0000;
const alloc_max: u64 = 0x7FFF_FFFF_FFFF;

pub const Flags = struct {
    pub const None = 0;
    pub const Present = 1 << 0;
    pub const Writable = 1 << 1;
    pub const User = 1 << 2;
    pub const WriteThrough = 1 << 3;
    pub const NoCache = 1 << 4;
    pub const NoExecute = 1 << 63;
};

fn switchPageTable(cr3: u64) void {
    asm volatile ("mov %[cr3], %%cr3"
        :
        : [cr3] "r" (cr3),
        : "memory"
    );
}

fn getPageTable(page_table: *PageTable, index: usize, allocate: bool) ?*PageTable {
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

fn virtToIndices(virt: u64) struct { pml4: usize, pml3: usize, pml2: usize, pml1: usize } {
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
        const indices = virtToIndices(virt_addr);
        const pml3 = getPageTable(self, indices.pml4, false) orelse return null;
        const pml2 = getPageTable(pml3, indices.pml3, false) orelse return null;
        const pml1 = getPageTable(pml2, indices.pml2, false) orelse return null;
        const entry = &pml1.entries[indices.pml1];

        if (entry.getFlags() & Flags.Present != 0) {
            return entry.getAddress();
        } else {
            return null;
        }
    }

    pub fn mapPage(self: *PageTable, virt_addr: u64, phys_addr: u64, flags: u64) !void {
        const indices = virtToIndices(virt_addr);
        const pml3 = getPageTable(self, indices.pml4, true) orelse return error.OutOfMemory;
        const pml2 = getPageTable(pml3, indices.pml3, true) orelse return error.OutOfMemory;
        const pml1 = getPageTable(pml2, indices.pml2, true) orelse return error.OutOfMemory;
        const entry = &pml1.entries[indices.pml1];

        if (entry.getFlags() & Flags.Present != 0) {
            return error.AlreadyMapped;
        } else {
            entry.setAddress(phys_addr);
            entry.setFlags(flags);
        }
    }

    pub fn remapPage(self: *PageTable, virt_addr: u64, phys_addr: u64, flags: u64) !void {
        const indices = virtToIndices(virt_addr);
        const pml3 = getPageTable(self, indices.pml4, false) orelse return error.OutOfMemory;
        const pml2 = getPageTable(pml3, indices.pml3, false) orelse return error.OutOfMemory;
        const pml1 = getPageTable(pml2, indices.pml2, false) orelse return error.OutOfMemory;
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
        const indices = virtToIndices(virt_addr);
        const pml3 = getPageTable(self, indices.pml4, false) orelse return error.OutOfMemory;
        const pml2 = getPageTable(pml3, indices.pml3, false) orelse return error.OutOfMemory;
        const pml1 = getPageTable(pml2, indices.pml2, false) orelse return error.OutOfMemory;
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

const Mapping = struct {
    protection: u64,
    flags: u64,
    start_addr: u64,
    end_addr: u64,
    file: ?*vfs.VNode = null,
    node: std.TailQueue(void).Node = undefined,
};

pub const FaultReason = enum {
    Read,
    Write,
    InstructionFetch,
};

pub const AddressSpace = struct {
    cr3: u64,
    page_table: *PageTable,
    mappings: std.TailQueue(void) = .{},

    pub fn init(cr3: u64) AddressSpace {
        return .{
            .cr3 = cr3,
            .page_table = @intToPtr(*PageTable, hhdm + cr3),
        };
    }

    pub fn switchTo(self: *AddressSpace) *AddressSpace {
        _ = paging_lock.lock();

        defer paging_lock.unlock();

        if (current_address_space) |previous| {
            if (self == previous) {
                return self;
            }

            current_address_space = self;

            switchPageTable(self.cr3);

            return previous;
        } else {
            switchPageTable(self.cr3);

            return self;
        }
    }

    pub fn handlePageFault(self: *AddressSpace, address: u64, reason: FaultReason) !bool {
        var current = self.mappings.first;

        while (current != null) |mapping| : (current = mapping.next) {
            _ = address;
            _ = reason;

            logger.debug("{}", .{mapping});

            if (mapping.next == self.mappings.first) {
                break;
            }
        }

        return false;
    }

    pub fn mmap(
        self: *AddressSpace,
        address: u64,
        size: usize,
        offset: usize,
        prot: u64,
        flags: u64,
        file: ?*vfs.VNode,
    ) !?u64 {
        if (size == 0 or !utils.isAligned(usize, offset, std.mem.page_size)) {
            return null;
        }

        var address_aligned = utils.alignDown(u64, address, std.mem.page_size);
        var size_aligned = utils.alignUp(usize, size, std.mem.page_size);

        if (address_aligned + size_aligned > alloc_max) {
            return null;
        }

        _ = self;
        _ = prot;
        _ = flags;

        if (file) |vnode| {
            logger.debug(
                "Attempting to map {} bytes of file {s} at offset {} at 0x{X}",
                .{ size_aligned, vnode.name, offset, address_aligned },
            );
        } else {
            logger.debug("Attempting to map {} bytes at 0x{X}", .{ size_aligned, address_aligned });
        }

        return null;
    }
};

pub var hhdm: u64 = 0;
pub var kernel_address_space: ?AddressSpace = null;

var paging_lock: IrqSpinlock = .{};
var current_address_space: ?*AddressSpace = null;

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
    const size = utils.alignUp(usize, @ptrToInt(end) - @ptrToInt(begin), std.mem.page_size);

    try page_table.map(virt_base, phys_base, size, flags);
}

pub fn init(hhdm_res: *limine.Hhdm.Response, kernel_addr_res: *limine.KernelAddress.Response) !void {
    const page_table_phys = phys.allocate(1, true) orelse return error.OutOfMemory;

    hhdm = hhdm_res.offset;
    kernel_address_space = AddressSpace.init(page_table_phys);

    // Prepopulate the higher half of kernel address space
    var page_table = kernel_address_space.?.page_table;
    var i: usize = 256;

    while (i < 512) : (i += 1) {
        _ = getPageTable(page_table, i, true);
    }

    try page_table.map(std.mem.page_size, std.mem.page_size, utils.gib(4) - std.mem.page_size, Flags.Present | Flags.Writable);
    try page_table.map(hhdm, 0, utils.gib(4), Flags.Present | Flags.Writable);

    try map_section("text", page_table, kernel_addr_res, Flags.Present);
    try map_section("rodata", page_table, kernel_addr_res, Flags.Present | Flags.NoExecute);
    try map_section("data", page_table, kernel_addr_res, Flags.Present | Flags.NoExecute | Flags.Writable);

    _ = kernel_address_space.?.switchTo();
}

pub fn createAddressSpace() !AddressSpace {
    var page_table_phys = phys.allocate(1, true) orelse return error.OutOfMemory;
    var address_space = AddressSpace.init(page_table_phys);

    var i: usize = 256;

    while (i < 512) : (i += 1) {
        address_space.page_table.entries[i] = kernel_address_space.?.page_table.entries[i];
    }

    return address_space;
}
