const std = @import("std");

const interrupts = @import("./interrupts.zig");

pub inline fn readEFlags() u64 {
    return asm volatile (
        \\pushf
        \\pop %[result]
        : [result] "=r" (-> u64),
    );
}

pub inline fn hlt() void {
    asm volatile ("hlt");
}

pub inline fn out(comptime T: type, port: u16, value: T) void {
    switch (T) {
        u8 => asm volatile ("outb %[val], %[port]"
            :
            : [val] "{al}" (value),
              [port] "N{dx}" (port),
        ),
        u16 => asm volatile ("outw %[val], %[port]"
            :
            : [val] "{ax}" (value),
              [port] "N{dx}" (port),
        ),
        u32 => asm volatile ("outl %[val], %[port]"
            :
            : [val] "{eax}" (value),
              [port] "N{dx}" (port),
        ),
        else => @compileError("No port out instruction is available for type " ++ @typeName(T)),
    }
}

pub inline fn in(comptime T: type, port: u16) T {
    return switch (T) {
        u8 => asm volatile ("inb %[port], %[result]"
            : [result] "={al}" (-> T),
            : [port] "N{dx}" (port),
        ),
        u16 => asm volatile ("inw %[port], %[result]"
            : [result] "={ax}" (-> T),
            : [port] "N{dx}" (port),
        ),
        u32 => asm volatile ("inl %[port], %[result]"
            : [result] "={eax}" (-> T),
            : [port] "N{dx}" (port),
        ),
        else => @compileError("No port in instruction is available for type " ++ @typeName(T)),
    };
}

pub const MSR = enum(u32) {
    IA32_APIC_BASE = 0x1B,
    IA32_FS_BASE = 0xC0000100,
    IA32_GS_BASE = 0xC0000101,
    IA32_KERNEL_GS_BASE = 0xC0000102,
    _,
};

pub inline fn rdmsr(msr: MSR) u64 {
    var low: u32 = undefined;
    var high: u32 = undefined;
    asm volatile ("rdmsr"
        : [_] "={eax}" (low),
          [_] "={edx}" (high),
        : [_] "{ecx}" (@intFromEnum(msr)),
    );
    return @as(u64, low) | (@as(u64, high) << 32);
}

pub inline fn wrmsr(msr: MSR, value: u64) void {
    asm volatile ("wrmsr"
        :
        : [_] "{eax}" (value & 0xFFFFFFFF),
          [_] "{edx}" (value >> 32),
          [_] "{ecx}" (@intFromEnum(msr)),
    );
}

const TableRegister = extern struct {
    limit: u16,
    base: u64 align(2),
};

pub const TSS = extern struct {
    reserved: u32,
    rsp: [3]u64 align(4),
    reserved0: u64 align(4),
    ist: [7]u64 align(4),
    reserved1: u32,
    reserved2: u32,
    reserved3: u16,
    iopb_offset: u16,
};

pub const IDT = struct {
    const Entry = packed struct(u128) {
        offset_low: u16,
        selector: u16,
        ist: u8,
        flags: u8,
        offset_mid: u16,
        offset_high: u32,
        reserved: u32,

        pub fn init(offset: u64, ist: u8, flags: u8) Entry {
            return .{
                .offset_low = @truncate(offset),
                .selector = 0x28,
                .ist = ist,
                .flags = flags,
                .offset_mid = @truncate(offset >> 16),
                .offset_high = @intCast(offset >> 32),
                .reserved = 0,
            };
        }
    };

    entries: [256]Entry = undefined,

    pub fn load(self: *@This(), has_tss: bool) void {
        const idtr: TableRegister = .{ .limit = @sizeOf(IDT) - 1, .base = @intFromPtr(self) };
        for (interrupts.getInterruptHandlers(), 0..) |handler, i| {
            const flags: u8 = 0x8E | if (i == interrupts.sched_call_vector) @as(u8, 3 << 5) else 0;
            const ist: u8 = if (has_tss) switch (i) {
                0xE => 3,
                interrupts.sched_call_vector => 2,
                else => 1,
            } else 0;
            self.entries[i] = Entry.init(@intFromPtr(handler), ist, flags);
        }
        asm volatile ("lidt (%[idtr])"
            :
            : [idtr] "r" (&idtr),
        );
    }
};

pub const GDT = struct {
    const Entry = packed struct(u64) {
        limit: u16,
        base_low: u16,
        base_mid: u8,
        flags: u8,
        granularity: u8,
        base_high: u8,
    };

    const SystemEntry = packed struct(u128) {
        lower: Entry,
        base_high_ex: u32,
        reserved: u32,
    };

    entries: [11]u64 = .{
        0x0000000000000000, // null
        0x00009A000000FFFF, // 16-bit code
        0x000093000000FFFF, // 16-bit data
        0x00CF9A000000FFFF, // 32-bit code
        0x00CF93000000FFFF, // 32-bit data
        0x00AF9B000000FFFF, // 64-bit code
        0x00AF93000000FFFF, // 64-bit data
        0x00AFFB000000FFFF, // usermode 64-bit code
        0x00AFF3000000FFFF, // usermode 64-bit data
        0x0000000000000000, // tss low
        0x0000000000000000, // tss high
    },

    pub fn load(self: *@This()) void {
        const gdtr: TableRegister = .{ .limit = @sizeOf(GDT) - 1, .base = @intFromPtr(self) };
        asm volatile (
            \\lgdt (%[gdtr])
            \\push $0x28
            \\lea 1f(%%rip), %%rax
            \\push %%rax
            \\lretq
            \\1:
            \\mov $0x30, %%ax
            \\mov %%ax, %%ds
            \\mov %%ax, %%es
            \\mov %%ax, %%ss
            :
            : [gdtr] "r" (&gdtr),
        );
    }

    pub fn loadTSS(self: *@This(), tss: *TSS) void {
        const tss_entry = @as(*SystemEntry, @ptrCast(&self.entries[9]));
        const tss_address = @intFromPtr(tss);
        tss_entry.* = .{
            .lower = .{
                .limit = @sizeOf(TSS) - 1,
                .base_low = @truncate(tss_address),
                .base_mid = @truncate(tss_address >> 16),
                .flags = 0b10001001,
                .granularity = 0,
                .base_high = @truncate(tss_address >> 24),
            },
            .base_high_ex = @intCast(tss_address >> 32),
            .reserved = 0,
        };
        asm volatile (
            \\mov $0x48, %%ax
            \\ltr %%ax
        );
    }
};
