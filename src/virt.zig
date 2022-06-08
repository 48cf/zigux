const logger = std.log.scoped(.virt);

const root = @import("root");
const std = @import("std");

const arch = @import("arch.zig");
const abi = @import("abi.zig");
const limine = @import("limine.zig");
const phys = @import("phys.zig");
const utils = @import("utils.zig");
const vfs = @import("vfs.zig");
const per_cpu = @import("per_cpu.zig");

const IrqSpinlock = @import("irq_lock.zig").IrqSpinlock;

const two_mib = utils.mib(2);
const user_alloc_base: u64 = 0x7000_0000_0000;
const user_alloc_max: u64 = 0x7FFF_FFFF_8000;
const flags_mask: u64 = 0xFFF0_0000_0000_0FFF;
const hhdm: u64 = 0xFFFF_8000_0000_0000;
const hhdm_uc: u64 = 0xFFFF_9000_0000_0000;

pub const Flags = struct {
    pub const None = 0;
    pub const Present = 1 << 0;
    pub const Writable = 1 << 1;
    pub const User = 1 << 2;
    pub const WriteThrough = 1 << 3;
    pub const NoCache = 1 << 4;
    pub const LargePage = 1 << 7;
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
        return asHigherHalf(*PageTable, entry.getAddress());
    } else if (allocate) {
        const new_page_table = phys.allocate(1, true) orelse return null;

        entry.setAddress(new_page_table);
        entry.setFlags(Flags.Present | Flags.Writable | Flags.User);

        return asHigherHalf(*PageTable, new_page_table);
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

inline fn protToFlags(prot: u64, user: bool) u64 {
    var flags: u64 = Flags.Present;

    if (user)
        flags |= Flags.User;

    if (prot & abi.PROT_WRITE != 0)
        flags |= Flags.Writable;

    if (prot & abi.PROT_EXEC == 0)
        flags |= Flags.NoExecute;

    return flags;
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
        const entry = if (flags & Flags.LargePage != 0)
            &pml2.entries[indices.pml2]
        else blk: {
            const pml1 = getPageTable(pml2, indices.pml2, true) orelse return error.OutOfMemory;
            break :blk &pml1.entries[indices.pml1];
        };

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
        const entry = if (flags & Flags.LargePage != 0)
            &pml2.entries[indices.pml2]
        else blk: {
            const pml1 = getPageTable(pml2, indices.pml2, true) orelse return error.OutOfMemory;
            break :blk &pml1.entries[indices.pml1];
        };

        std.debug.assert((entry.getFlags() & Flags.LargePage) == (flags & Flags.LargePage));

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

    pub fn unmapPage(self: *PageTable, virt_addr: u64, large_page: bool) !void {
        const indices = virtToIndices(virt_addr);
        const pml3 = getPageTable(self, indices.pml4, false) orelse return error.OutOfMemory;
        const pml2 = getPageTable(pml3, indices.pml3, false) orelse return error.OutOfMemory;
        const pml2_entry = &pml2.entries[indices.pml2];
        const entry = if (pml2_entry.getFlags() & Flags.Present == 0) {
            return error.NotMapped;
        } else if (pml2_entry.getFlags() & Flags.LargePage != 0) blk: {
            if (large_page) {
                break :blk &pml2.entries[indices.pml2];
            } else {
                return error.LargePage;
            }
        } else blk: {
            std.debug.assert(!large_page);

            const pml1 = getPageTable(pml2, indices.pml2, true) orelse return error.OutOfMemory;
            break :blk &pml1.entries[indices.pml1];
        };

        if (entry.getFlags() & Flags.Present != 0) {
            entry.setAddress(0);
            entry.setFlags(Flags.None);

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

        std.debug.assert(utils.isAligned(u64, virt_addr, std.mem.page_size));
        std.debug.assert(utils.isAligned(u64, phys_addr, std.mem.page_size));
        std.debug.assert(utils.isAligned(u64, size, std.mem.page_size));

        while (i < size) {
            const new_virt_addr = virt_addr + i;
            const new_phys_addr = phys_addr + i;

            if (utils.isAligned(u64, new_virt_addr, two_mib) and utils.isAligned(u64, new_phys_addr, two_mib) and size - i >= two_mib) {
                try self.mapPage(new_virt_addr, new_phys_addr, flags | Flags.LargePage);
                i += two_mib;
            } else {
                try self.mapPage(new_virt_addr, new_phys_addr, flags);
                i += std.mem.page_size;
            }
        }
    }

    pub fn remap(self: *PageTable, virt_addr: u64, phys_addr: u64, size: usize, flags: u64) !void {
        var i: u64 = 0;

        std.debug.assert(utils.isAligned(u64, virt_addr, std.mem.page_size));
        std.debug.assert(utils.isAligned(u64, phys_addr, std.mem.page_size));
        std.debug.assert(utils.isAligned(u64, size, std.mem.page_size));

        while (i < size) {
            const new_virt_addr = virt_addr + i;
            const new_phys_addr = phys_addr + i;

            if (utils.isAligned(u64, new_virt_addr, two_mib) and utils.isAligned(u64, new_phys_addr, two_mib) and size - i >= two_mib) {
                try self.remapPage(new_virt_addr, new_phys_addr, flags | Flags.LargePage);
                i += two_mib;
            } else {
                try self.remapPage(new_virt_addr, new_phys_addr, flags);
                i += std.mem.page_size;
            }
        }
    }

    pub fn unmap(self: *PageTable, virt_addr: u64, size: usize) !void {
        var i: u64 = 0;

        std.debug.assert(utils.isAligned(u64, virt_addr, std.mem.page_size));
        std.debug.assert(utils.isAligned(u64, size, std.mem.page_size));

        while (i < size) {
            const new_virt_addr = virt_addr + i;

            if (utils.isAligned(u64, new_virt_addr, two_mib) and size - i >= two_mib) {
                try self.unmapPage(new_virt_addr, true);
                i += two_mib;
            } else {
                try self.unmapPage(new_virt_addr, false);
                i += std.mem.page_size;
            }
        }
    }
};

pub const LoadedExecutable = struct {
    entry: u64,
    ld_path: ?[]const u8,
    aux_vals: struct {
        at_entry: u64,
        at_phdr: u64,
        at_phent: u64,
        at_phnum: u64,
    },
};

pub const FaultReason = struct {
    pub const Present = 1 << 0;
    pub const Write = 1 << 1;
    pub const User = 1 << 2;
    pub const InstructionFetch = 1 << 4;
};

pub const Mapping = struct {
    base: u64,
    length: u64,
    prot: u64,
    flags: u64,
    node: std.TailQueue(void).Node = undefined,
};

pub const AddressSpace = struct {
    cr3: u64,
    page_table: *PageTable,
    lock: IrqSpinlock = .{},
    mappings: std.TailQueue(void) = .{},
    alloc_base: u64 = user_alloc_max,

    pub fn init(cr3: u64) AddressSpace {
        return .{
            .cr3 = cr3,
            .page_table = asHigherHalf(*PageTable, cr3),
        };
    }

    pub fn switchTo(self: *AddressSpace) *AddressSpace {
        _ = paging_lock.lock();

        defer paging_lock.unlock();

        const previous = current_address_space;

        if (current_cr3 != self.cr3) {
            switchPageTable(self.cr3);

            current_address_space = self;
            current_cr3 = self.cr3;
        }

        return previous;
    }

    pub fn loadExecutable(self: *AddressSpace, file: *vfs.VNode, base: u64) !LoadedExecutable {
        var stream = file.stream();
        var header = try std.elf.Header.read(&stream);
        var ph_iter = header.program_header_iterator(&stream);
        var ld_path: ?[]u8 = null;
        var entry: u64 = header.entry + base;
        var phdr: u64 = 0;

        logger.debug("Trying to load executable {} at 0x{X}", .{ file.getFullPath(), base });

        while (try ph_iter.next()) |ph| {
            switch (ph.p_type) {
                std.elf.PT_INTERP => {
                    ld_path = try root.allocator.alloc(u8, ph.p_filesz);
                    _ = try file.read(ld_path.?, ph.p_offset);
                },
                std.elf.PT_PHDR => phdr = ph.p_vaddr + base,
                std.elf.PT_LOAD => {
                    logger.debug(
                        "  PT_LOAD {{ .p_offset=0x{X}, .p_vaddr=0x{X}, .p_filesz=0x{X}, .p_memsz=0x{X}, .p_flags=0x{X} }}",
                        .{ ph.p_offset, ph.p_vaddr, ph.p_filesz, ph.p_memsz, ph.p_flags },
                    );

                    const misalign = ph.p_vaddr & (std.mem.page_size - 1);
                    const page_count = utils.divRoundUp(u64, misalign + ph.p_memsz, std.mem.page_size);
                    const page_phys = phys.allocate(page_count, true) orelse return error.OutOfMemory;
                    const page_hh = asHigherHalf([*]u8, page_phys + misalign);

                    var prot: u64 = 0;

                    if (ph.p_flags & std.elf.PF_R != 0)
                        prot |= abi.PROT_READ;

                    if (ph.p_flags & std.elf.PF_W != 0)
                        prot |= abi.PROT_WRITE;

                    if (ph.p_flags & std.elf.PF_X != 0)
                        prot |= abi.PROT_EXEC;

                    const virt_addr = utils.alignDown(u64, ph.p_vaddr, std.mem.page_size) + base;
                    const mapping = try root.allocator.create(Mapping);

                    try self.page_table.map(virt_addr, page_phys, page_count * std.mem.page_size, protToFlags(prot, true));
                    _ = try file.read(page_hh[0..ph.p_filesz], ph.p_offset);

                    mapping.* = .{
                        .base = virt_addr,
                        .length = page_count * std.mem.page_size,
                        .prot = prot,
                        .flags = abi.MAP_ANONYMOUS | abi.MAP_PRIVATE | abi.MAP_FIXED,
                    };

                    self.insertMapping(mapping);
                },
                else => continue,
            }
        }

        return LoadedExecutable{
            .entry = entry,
            .ld_path = if (ld_path) |path| blk: {
                const null_term = @ptrCast([*:0]const u8, path);

                break :blk null_term[0..std.mem.len(null_term)];
            } else null,
            .aux_vals = .{
                .at_entry = entry,
                .at_phdr = phdr,
                .at_phent = header.phentsize,
                .at_phnum = header.phnum,
            },
        };
    }

    pub fn handlePageFault(self: *AddressSpace, address: u64, reason: u64) !bool {
        _ = self.lock.lock();

        defer self.lock.unlock();

        if (reason & FaultReason.Present != 0) {
            return false;
        }

        var iter = self.mappings.first;

        while (iter) |node| : (iter = node.next) {
            const mapping = @fieldParentPtr(Mapping, "node", node);

            if (address >= mapping.base and address < mapping.base + mapping.length) {
                const base = utils.alignDown(u64, address, std.mem.page_size);
                const page_phys = phys.allocate(1, true) orelse return error.OutOfMemory;
                const flags = protToFlags(mapping.prot, true);

                try self.page_table.mapPage(base, page_phys, flags);

                return true;
            }
        }

        return false;
    }

    pub fn mmap(
        self: *AddressSpace,
        hint: u64,
        length: usize,
        prot: u64,
        flags: u64,
        file: ?*vfs.VNode,
        offset: usize,
    ) !u64 {
        std.debug.assert(file == null);
        std.debug.assert(offset == 0);

        if (hint != 0 and hint >= user_alloc_base) {
            return error.InvalidArgument;
        }

        var address = utils.alignDown(u64, hint, std.mem.page_size);
        var size = utils.alignUp(u64, length, std.mem.page_size);

        if (address == 0) {
            self.alloc_base -= size;

            address = self.alloc_base;
        }

        const mapping = try root.allocator.create(Mapping);

        mapping.* = .{
            .base = address,
            .length = size,
            .prot = prot,
            .flags = flags,
        };

        self.insertMapping(mapping);

        return address;
    }

    pub fn fork(self: *AddressSpace) !AddressSpace {
        _ = self.lock.lock();

        defer self.lock.unlock();

        var new_as = try createAddressSpace();

        _ = new_as.lock.lock();

        defer new_as.lock.unlock();

        new_as.alloc_base = self.alloc_base;

        var iter = self.mappings.first;

        while (iter) |node| : (iter = node.next) {
            const mapping = @fieldParentPtr(Mapping, "node", node);
            const new_mapping = try root.allocator.create(Mapping);

            new_mapping.* = mapping.*;
            new_as.insertMapping(new_mapping);

            // TODO: Please someone implement CoW for me :sadge:
            for (utils.range(utils.alignUp(usize, mapping.length, std.mem.page_size) / std.mem.page_size)) |_, page_index| {
                const original_page = self.page_table.translate(mapping.base + page_index * std.mem.page_size) orelse continue;
                const new_page = phys.allocate(1, true) orelse return error.OutOfMemory;

                std.mem.copy(
                    u8,
                    asHigherHalf([*]u8, new_page)[0..std.mem.page_size],
                    asHigherHalf([*]u8, original_page)[0..std.mem.page_size],
                );

                try new_as.page_table.mapPage(
                    mapping.base + page_index * std.mem.page_size,
                    new_page,
                    protToFlags(mapping.prot, true),
                );
            }
        }

        return new_as;
    }

    fn insertMapping(self: *AddressSpace, new_mapping: *Mapping) void {
        var iter = self.mappings.first;

        while (iter) |node| : (iter = node.next) {
            const mapping = @fieldParentPtr(Mapping, "node", node);

            if (new_mapping.base < mapping.base) {
                std.debug.assert(new_mapping.base + new_mapping.length <= mapping.base);

                self.mappings.insertBefore(node, &new_mapping.node);

                return;
            }
        }

        self.mappings.append(&new_mapping.node);
    }
};

pub var kernel_address_space: AddressSpace = undefined;

var paging_lock: IrqSpinlock = .{};
var current_address_space: *AddressSpace = undefined;
var current_cr3: u64 = undefined;

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

pub fn init(kernel_addr_res: *limine.KernelAddress.Response) !void {
    const page_table_phys = phys.allocate(1, true) orelse return error.OutOfMemory;
    const page_table = asHigherHalf(*PageTable, page_table_phys);

    // Pre-populate the higher half of kernel address space
    var i: usize = 256;

    while (i < 512) : (i += 1) {
        _ = getPageTable(page_table, i, true);
    }

    // TODO: Map all of the memory map entries too
    try page_table.map(std.mem.page_size, std.mem.page_size, utils.gib(16) - std.mem.page_size, Flags.Present | Flags.Writable);
    try page_table.map(hhdm, 0, utils.gib(16), Flags.Present | Flags.Writable | Flags.NoExecute);

    try map_section("text", page_table, kernel_addr_res, Flags.Present);
    try map_section("rodata", page_table, kernel_addr_res, Flags.Present | Flags.NoExecute);
    try map_section("data", page_table, kernel_addr_res, Flags.Present | Flags.NoExecute | Flags.Writable);

    // Prepare for the address space switch
    kernel_address_space = AddressSpace.init(page_table_phys);

    current_address_space = &kernel_address_space;
    current_cr3 = page_table_phys;

    switchPageTable(current_cr3);
}

pub fn createAddressSpace() !AddressSpace {
    var page_table_phys = phys.allocate(1, true) orelse return error.OutOfMemory;
    var address_space = AddressSpace.init(page_table_phys);

    var i: usize = 256;

    while (i < 512) : (i += 1) {
        address_space.page_table.entries[i] = kernel_address_space.page_table.entries[i];
    }

    return address_space;
}

pub fn handlePageFault(address: u64, reason: u64) !bool {
    const page = utils.alignDown(u64, address, std.mem.page_size);

    // TODO: Map all of the memory map entries too
    if (address >= hhdm_uc and address < hhdm_uc + utils.gib(16)) {
        try kernel_address_space.page_table.mapPage(
            page,
            page - hhdm_uc,
            Flags.Present | Flags.Writable | Flags.NoCache | Flags.NoExecute,
        );

        return true;
    } else if (address < 0x8000_0000_0000_0000) {
        return current_address_space.handlePageFault(address, reason);
    }

    return false;
}

pub fn asHigherHalf(comptime T: type, addr: u64) T {
    const result = addr + hhdm;

    if (@typeInfo(T) == .Pointer) {
        return @intToPtr(T, result);
    } else {
        return @as(T, result);
    }
}

pub fn asHigherHalfUncached(comptime T: type, addr: u64) T {
    const result = addr + hhdm_uc;

    if (@typeInfo(T) == .Pointer) {
        return @intToPtr(T, result);
    } else {
        return @as(T, result);
    }
}
