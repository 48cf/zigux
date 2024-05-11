const root = @import("root");
const std = @import("std");

const phys = @import("./phys.zig");
const virt = @import("./virt.zig");

pub inline fn kib(comptime value: comptime_int) comptime_int {
    return value * 1024;
}

pub inline fn mib(comptime value: comptime_int) comptime_int {
    return kib(value) * 1024;
}

pub inline fn gib(comptime value: comptime_int) comptime_int {
    return mib(value) * 1024;
}

pub inline fn tib(comptime value: comptime_int) comptime_int {
    return gib(value) * 1024;
}

pub const KernelStack = struct {
    base: u64,
    size: usize,

    pub fn allocate(pages: usize) !@This() {
        const address = root.kernel_va_arena.allocate((pages + 1) * std.mem.page_size) orelse
            return error.OutOfMemory;

        try virt.kernel_address_space.page_table.mapPage(address, 0, virt.PTEFlags.guard_page);

        for (0..pages) |i| {
            const phys_addr = phys.allocate(1, false) orelse return error.NoMemory;
            try virt.kernel_address_space.page_table.mapPage(
                address + (i + 1) * std.mem.page_size,
                phys_addr,
                virt.PTEFlags.present | virt.PTEFlags.writable,
            );
        }

        return .{
            .base = address + std.mem.page_size,
            .size = pages * std.mem.page_size,
        };
    }

    pub fn getEndAddress(self: *const @This()) u64 {
        return self.base + self.size;
    }

    pub fn deinit(self: @This()) void {
        try virt.kernel_address_space.page_table.unmap(self.base, self.size + std.mem.page_size);
        root.kernel_va_arena.free(self.base - std.mem.page_size, self.size + std.mem.page_size);
    }
};
