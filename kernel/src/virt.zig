const logger = std.log.scoped(.virt);

const root = @import("root");
const std = @import("std");
const limine = @import("limine");

const arch = @import("arch.zig");
const abi = @import("abi.zig");
const phys = @import("phys.zig");
const utils = @import("utils.zig");
const vfs = @import("vfs.zig");
const per_cpu = @import("per_cpu.zig");
const lock = @import("lock.zig");

const two_mib = utils.mib(2);
const user_alloc_base: u64 = 0x7000_0000_0000;
const user_alloc_max: u64 = 0x7FFF_FFFF_8000;
const flags_mask: u64 = 0xFFF0_0000_0000_0FFF;
const hhdm: u64 = 0xFFFF_8000_0000_0000;
const hhdm_uc: u64 = 0xFFFF_9000_0000_0000;

pub const PTEFlags = struct {
    pub const none = 0;
    pub const present = 1 << 0;
    pub const writable = 1 << 1;
    pub const user = 1 << 2;
    pub const write_through = 1 << 3;
    pub const no_cache = 1 << 4;
    pub const no_execute = 1 << 63;
};

fn switchPageTable(cr3: u64) void {
    asm volatile ("mov %[cr3], %%cr3"
        :
        : [cr3] "r" (cr3),
        : "memory"
    );
}

fn getPageTable(pt: *PageTable, index: u9, allocate: bool) ?*PageTable {
    var entry = &pt.entries[index];
    if ((entry.getFlags() & PTEFlags.present) != 0) {
        return asHigherHalf(*PageTable, entry.getAddress());
    }
    if (allocate) {
        const new_pt_phys = phys.allocate(1, true) orelse return null;
        entry.setAddress(new_pt_phys);
        entry.setFlags(PTEFlags.present | PTEFlags.writable | PTEFlags.user);
        return asHigherHalf(*PageTable, new_pt_phys);
    }
    return null;
}

fn addressToIndices(address: u64) [4]u9 {
    return .{
        @truncate(address >> 39), // PML4 index
        @truncate(address >> 30), // PML3 index
        @truncate(address >> 21), // PML2 index
        @truncate(address >> 12), // PML1 index
    };
}

inline fn protToPTEFlags(prot: u64, user: bool) u64 {
    return PTEFlags.present |
        if (user) PTEFlags.user else 0 |
        if ((prot & abi.PROT_WRITE) != 0) PTEFlags.write else 0 |
        if ((prot & abi.PROT_EXEC) == 0) PTEFlags.no_execute else 0;
}

const PTE = extern struct {
    value: u64,

    pub fn getAddress(self: *const PTE) u64 {
        return self.value & ~flags_mask;
    }

    pub fn getFlags(self: *const PTE) u64 {
        return self.value & flags_mask;
    }

    pub fn setAddress(self: *PTE, address: u64) void {
        self.value = address | self.getFlags();
    }

    pub fn setFlags(self: *PTE, flags: u64) void {
        self.value = self.getAddress() | flags;
    }
};

const PageTable = extern struct {
    entries: [512]PTE,

    pub fn translate(self: *PageTable, address: u64) ?u64 {
        const pml4i, const pml3i, const pml2i, const pml1i = addressToIndices(address);
        const pml3 = getPageTable(self, pml4i, false) orelse return null;
        const pml2 = getPageTable(pml3, pml3i, false) orelse return null;
        const pml1 = getPageTable(pml2, pml2i, false) orelse return null;
        const entry = &pml1.entries[pml1i];
        if ((entry.getFlags() & PTEFlags.present) != 0) {
            return entry.getAddress();
        }
        return null;
    }

    pub fn mapPage(self: *PageTable, address: u64, physical: u64, flags: u64) !void {
        const pml4i, const pml3i, const pml2i, const pml1i = addressToIndices(address);
        const pml3 = getPageTable(self, pml4i, true) orelse return error.OutOfMemory;
        const pml2 = getPageTable(pml3, pml3i, true) orelse return error.OutOfMemory;
        const pml1 = getPageTable(pml2, pml2i, true) orelse return error.OutOfMemory;
        const entry = &pml1.entries[pml1i];
        if ((entry.getFlags() & PTEFlags.present) != 0) {
            return error.AlreadyMapped;
        }
        entry.setAddress(physical);
        entry.setFlags(flags);
    }

    pub fn unmapPage(self: *PageTable, address: u64) !void {
        const pml4i, const pml3i, const pml2i, const pml1i = addressToIndices(address);
        const pml3 = getPageTable(self, pml4i, false) orelse return error.NotMapped;
        const pml2 = getPageTable(pml3, pml3i, false) orelse return error.NotMapped;
        const pml1 = getPageTable(pml2, pml2i, false) orelse return error.NotMapped;
        const entry = &pml1.entries[pml1i];
        if ((entry.getFlags() & PTEFlags.present) == 0) {
            return error.NotMapped;
        }
        entry.setAddress(0);
        entry.setFlags(PTEFlags.none);
        asm volatile ("invlpg %[page]"
            :
            : [page] "rm" (address),
        );
    }

    pub fn map(self: *PageTable, address: u64, physical: u64, size: usize, flags: u64) !void {
        std.debug.assert(std.mem.isAlignedGeneric(u64, address, std.mem.page_size));
        std.debug.assert(std.mem.isAlignedGeneric(u64, physical, std.mem.page_size));
        std.debug.assert(std.mem.isAlignedGeneric(u64, size, std.mem.page_size));
        var i: usize = 0;
        while (i < size) : (i += std.mem.page_size) {
            try self.mapPage(address + i, physical + i, flags);
        }
    }

    pub fn unmap(self: *PageTable, address: u64, size: usize) !void {
        std.debug.assert(std.mem.isAlignedGeneric(u64, address, std.mem.page_size));
        std.debug.assert(std.mem.isAlignedGeneric(u64, size, std.mem.page_size));
        var i: usize = 0;
        while (i < size) : (i += std.mem.page_size) {
            try self.unmapPage(address + i);
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

pub const PageFaultReason = struct {
    pub const present = 1 << 0;
    pub const write = 1 << 1;
    pub const user = 1 << 2;
    pub const instruction_fetch = 1 << 4;
};

pub const Mapping = struct {
    node: std.TailQueue(void).Node = .{ .data = {} },
    base: u64,
    length: u64,
    prot: u64,
    flags: u64,
};

pub const AddressSpace = struct {
    cr3: u64,
    page_table: *PageTable,
    lock: lock.Spinlock = .{},
    mappings: std.TailQueue(void) = .{},
    alloc_base: u64 = user_alloc_max,

    pub fn init(cr3: u64) AddressSpace {
        return .{
            .cr3 = cr3,
            .page_table = asHigherHalf(*PageTable, cr3),
        };
    }

    pub fn switchTo(self: *AddressSpace) *AddressSpace {
        paging_lock.lock();
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
        var phdr: u64 = 0;

        logger.debug("Trying to load executable {} at 0x{X}", .{ file.getFullPath(), base });

        while (try ph_iter.next()) |ph| {
            switch (ph.p_type) {
                std.elf.PT_INTERP => {
                    ld_path = try root.allocator.alloc(u8, ph.p_filesz);
                    _ = try file.read(ld_path.?, ph.p_offset, 0);
                },
                std.elf.PT_PHDR => phdr = ph.p_vaddr + base,
                std.elf.PT_LOAD => {
                    logger.debug(
                        "  PT_LOAD {{ .p_offset=0x{X}, .p_vaddr=0x{X}, .p_filesz=0x{X}, .p_memsz=0x{X}, .p_flags=0x{X} }}",
                        .{ ph.p_offset, ph.p_vaddr, ph.p_filesz, ph.p_memsz, ph.p_flags },
                    );

                    const misalign = ph.p_vaddr & (std.mem.page_size - 1);
                    const page_count = std.mem.alignForward(u64, misalign + ph.p_memsz, std.mem.page_size) / std.mem.page_size;
                    const page_phys = phys.allocate(page_count, true) orelse return error.OutOfMemory;
                    const page_hh = asHigherHalf([*]u8, page_phys + misalign);

                    var prot: u64 = 0;

                    if (ph.p_flags & std.elf.PF_R != 0)
                        prot |= abi.PROT_READ;

                    if (ph.p_flags & std.elf.PF_W != 0)
                        prot |= abi.PROT_WRITE;

                    if (ph.p_flags & std.elf.PF_X != 0)
                        prot |= abi.PROT_EXEC;

                    const virt_addr = std.mem.alignBackward(u64, ph.p_vaddr, std.mem.page_size) + base;
                    const mapping = try root.allocator.create(Mapping);

                    try self.page_table.map(virt_addr, page_phys, page_count * std.mem.page_size, protToPTEFlags(prot, true));
                    _ = try file.read(page_hh[0..ph.p_filesz], ph.p_offset, 0);

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
            .entry = header.entry + base,
            .ld_path = if (ld_path) |path| blk: {
                const null_term = @as([*:0]const u8, @ptrCast(path));

                break :blk null_term[0..std.mem.len(null_term)];
            } else null,
            .aux_vals = .{
                .at_entry = header.entry + base,
                .at_phdr = phdr,
                .at_phent = header.phentsize,
                .at_phnum = header.phnum,
            },
        };
    }

    pub fn handlePageFault(self: *AddressSpace, address: u64, reason: u64) !bool {
        self.lock.lock();
        defer self.lock.unlock();

        if (reason & PageFaultReason.present != 0) {
            return false;
        }

        var iter = self.mappings.first;

        while (iter) |node| : (iter = node.next) {
            const mapping = @as(*Mapping, @fieldParentPtr("node", node));

            if (address >= mapping.base and address < mapping.base + mapping.length) {
                const base = std.mem.alignBackward(u64, address, std.mem.page_size);
                const page_phys = phys.allocate(1, true) orelse return error.OutOfMemory;
                const flags = protToPTEFlags(mapping.prot, true);

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

        var address = std.mem.alignBackward(u64, hint, std.mem.page_size);
        const size = std.mem.alignForward(u64, length, std.mem.page_size);

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
        self.lock.lock();
        defer self.lock.unlock();

        var new_as = try createAddressSpace();

        new_as.lock.lock();
        defer new_as.lock.unlock();

        new_as.alloc_base = self.alloc_base;

        var iter = self.mappings.first;

        while (iter) |node| : (iter = node.next) {
            const mapping = @as(*Mapping, @fieldParentPtr("node", node));
            const new_mapping = try root.allocator.create(Mapping);

            new_mapping.* = mapping.*;
            new_as.insertMapping(new_mapping);

            // TODO: Please someone implement CoW for me :sadge:
            for (0..utils.alignUp(usize, mapping.length, std.mem.page_size) / std.mem.page_size) |page_index| {
                const original_page = self.page_table.translate(mapping.base + page_index * std.mem.page_size) orelse continue;
                const new_page = phys.allocate(1, true) orelse return error.OutOfMemory;

                @memcpy(
                    asHigherHalf([*]u8, new_page)[0..std.mem.page_size],
                    asHigherHalf([*]u8, original_page)[0..std.mem.page_size],
                );

                try new_as.page_table.mapPage(
                    mapping.base + page_index * std.mem.page_size,
                    new_page,
                    protToPTEFlags(mapping.prot, true),
                );
            }
        }

        return new_as;
    }

    fn insertMapping(self: *AddressSpace, new_mapping: *Mapping) void {
        var iter = self.mappings.first;

        while (iter) |node| : (iter = node.next) {
            const mapping = @as(*Mapping, @fieldParentPtr("node", node));

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

var paging_lock: lock.Spinlock = .{};
var current_address_space: *AddressSpace = undefined;
var current_cr3: u64 = undefined;

fn map_section(
    comptime section_name: []const u8,
    page_table: *PageTable,
    kernel_addr_res: *limine.KernelAddressResponse,
    flags: u64,
) !void {
    const begin = @extern(*u8, .{ .name = section_name ++ "_begin" });
    const end = @extern(*u8, .{ .name = section_name ++ "_end" });

    const start_addr = std.mem.alignBackward(usize, @intFromPtr(begin), std.mem.page_size);
    const end_addr = std.mem.alignForward(usize, @intFromPtr(end), std.mem.page_size);

    try page_table.map(start_addr, start_addr - kernel_addr_res.virtual_base + kernel_addr_res.physical_base, end_addr - start_addr, flags);
}

pub fn init(kernel_addr_res: *limine.KernelAddressResponse) !void {
    const page_table_phys = phys.allocate(1, true) orelse return error.OutOfMemory;
    const page_table = asHigherHalf(*PageTable, page_table_phys);

    // Pre-populate the higher half of kernel address space
    for (256..512) |i| {
        _ = getPageTable(page_table, @intCast(i), true);
    }

    // TODO: Map all of the memory map entries too
    try page_table.map(std.mem.page_size, std.mem.page_size, utils.gib(16) - std.mem.page_size, PTEFlags.present | PTEFlags.writable);
    try page_table.map(hhdm, 0, utils.gib(16), PTEFlags.present | PTEFlags.writable | PTEFlags.no_execute);
    try page_table.map(hhdm_uc, 0, utils.gib(16), PTEFlags.present | PTEFlags.writable | PTEFlags.no_cache | PTEFlags.no_execute);

    try map_section("text", page_table, kernel_addr_res, PTEFlags.present);
    try map_section("rodata", page_table, kernel_addr_res, PTEFlags.present | PTEFlags.no_execute);
    try map_section("data", page_table, kernel_addr_res, PTEFlags.present | PTEFlags.no_execute | PTEFlags.writable);

    // Prepare for the address space switch
    kernel_address_space = AddressSpace.init(page_table_phys);

    current_address_space = &kernel_address_space;
    current_cr3 = page_table_phys;

    switchPageTable(current_cr3);
}

pub fn createAddressSpace() !AddressSpace {
    const page_table_phys = phys.allocate(1, true) orelse return error.OutOfMemory;
    var address_space = AddressSpace.init(page_table_phys);

    for (256..512) |i| {
        address_space.page_table.entries[i] = kernel_address_space.page_table.entries[i];
    }

    return address_space;
}

pub fn handlePageFault(address: u64, reason: u64) !bool {
    if (address < 0x8000_0000_0000_0000) {
        return current_address_space.handlePageFault(address, reason);
    }

    return false;
}

pub fn higherHalfToPhysical(addr: anytype) u64 {
    if (@typeInfo(@TypeOf(addr)) == .Pointer) {
        return @intFromPtr(addr) - hhdm;
    }

    return @as(u64, addr) - hhdm;
}

pub fn higherHalfUncachedToPhysical(addr: anytype) u64 {
    if (@typeInfo(@TypeOf(addr)) == .Pointer) {
        return @intFromPtr(addr) - hhdm_uc;
    }

    return @as(u64, addr) - hhdm_uc;
}

pub fn asHigherHalf(comptime T: type, addr: u64) T {
    const result = addr + hhdm;

    if (@typeInfo(T) == .Pointer) {
        return @as(T, @ptrFromInt(result));
    } else {
        return @as(T, result);
    }
}

pub fn asHigherHalfUncached(comptime T: type, addr: u64) T {
    const result = addr + hhdm_uc;

    if (@typeInfo(T) == .Pointer) {
        return @as(T, @ptrFromInt(result));
    } else {
        return @as(T, result);
    }
}
