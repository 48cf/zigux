const logger = std.log.scoped(.arch);

const std = @import("std");

const interrupts = @import("interrupts.zig");

pub inline fn readEflags() u64 {
    return asm volatile (
        \\pushf
        \\pop %[result]
        : [result] "=r" (-> u64),
    );
}

pub inline fn halt() void {
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

pub const Msr = struct {
    msr: u32,

    pub const apic = Msr.init(0x1b);
    pub const fs_base = Msr.init(0xc0000100);
    pub const gs_base = Msr.init(0xc0000101);
    pub const gs_kernel_base = Msr.init(0xc0000102);

    fn init(msr: u32) Msr {
        return .{ .msr = msr };
    }

    pub fn read(self: Msr) u64 {
        var low: u32 = undefined;
        var high: u32 = undefined;

        asm volatile ("rdmsr"
            : [_] "={eax}" (low),
              [_] "={edx}" (high),
            : [_] "{ecx}" (self.msr),
        );

        return @as(u64, low) | (@as(u64, high) << 32);
    }

    pub fn write(self: Msr, value: u64) void {
        asm volatile ("wrmsr"
            :
            : [_] "{eax}" (value & 0xFFFFFFFF),
              [_] "{edx}" (value >> 32),
              [_] "{ecx}" (self.msr),
        );
    }
};

pub const Tss = packed struct {
    reserved: u32 = 0,
    rsp: [3]u64 = .{ 0, 0, 0 },
    reserved0: u64 = 0,
    ist: [7]u64 = .{ 0, 0, 0, 0, 0, 0, 0 },
    reserved1: u32 = 0,
    reserved2: u32 = 0,
    reserved3: u16 = 0,
    iopb_offset: u16 = 0,
};

pub const Idt = struct {
    entries: [256]IdtEntry = undefined,

    pub fn load(self: *Idt) void {
        const idtr = DescriptorTableRegister{
            .limit = @sizeOf(Idt) - 1,
            .base = @ptrToInt(self),
        };

        for (interrupts.makeHandlers()) |handler, i| {
            const ist: u8 = if (i == interrupts.sched_call_vector) 1 else 0;
            const flags: u8 = if (i == interrupts.syscall_vector) 0xee else 0x8e;

            self.entries[i] = IdtEntry.init(@ptrToInt(handler), ist, flags);
        }

        asm volatile ("lidt (%[idtr])"
            :
            : [idtr] "r" (&idtr),
        );
    }
};

pub const Gdt = struct {
    entries: [11]u64 = .{
        0x0000000000000000, // null
        0x00009a000000ffff, // 16-bit code
        0x000093000000ffff, // 16-bit data
        0x00cf9a000000ffff, // 32-bit code
        0x00cf93000000ffff, // 32-bit data
        0x00af9b000000ffff, // 64-bit code
        0x00af93000000ffff, // 64-bit data
        0x00affb000000ffff, // usermode 64-bit code
        0x00aff3000000ffff, // usermode 64-bit data
        0x0000000000000000, // tss low
        0x0000000000000000, // tss high
    },

    pub fn load(self: *Gdt, tss: *Tss) void {
        const tss_entry = @ptrCast(*GdtEntryExtended, &self.entries[9]);
        const tss_address = @ptrToInt(tss);

        tss_entry.* = .{
            .lower = .{
                .limit = @sizeOf(Tss) - 1,
                .base_low = @truncate(u16, tss_address),
                .base_mid = @truncate(u8, tss_address >> 16),
                .flags = 0b10001001,
                .granularity = 0,
                .base_high = @truncate(u8, tss_address >> 24),
            },
            .base_high_ex = @truncate(u32, tss_address >> 32),
            .reserved = 0,
        };

        const gdtr = DescriptorTableRegister{
            .limit = @sizeOf(Gdt) - 1,
            .base = @ptrToInt(self),
        };

        asm volatile (
            \\lgdt (%[gdtr])
            \\ltr %[tss_sel]
            :
            : [gdtr] "r" (&gdtr),
              [tss_sel] "r" (@as(u16, 0x48)),
        );
    }
};

const IdtEntry = extern struct {
    offset_low: u16,
    selector: u16,
    ist: u8,
    flags: u8,
    offset_mid: u16,
    offset_high: u32,
    reserved: u32,

    pub fn init(offset: u64, ist: u8, flags: u8) IdtEntry {
        return .{
            .offset_low = @truncate(u16, offset),
            .selector = 0x28,
            .ist = ist,
            .flags = flags,
            .offset_mid = @truncate(u16, offset >> 16),
            .offset_high = @truncate(u32, offset >> 32),
            .reserved = 0,
        };
    }
};

const GdtEntry = extern struct {
    limit: u16,
    base_low: u16,
    base_mid: u8,
    flags: u8,
    granularity: u8,
    base_high: u8,
};

const GdtEntryExtended = extern struct {
    lower: GdtEntry,
    base_high_ex: u32,
    reserved: u32,
};

const DescriptorTableRegister = packed struct {
    limit: u16,
    base: u64,
};

comptime {
    std.debug.assert(@sizeOf(IdtEntry) == 16);
    std.debug.assert(@sizeOf(GdtEntry) == 8);
    std.debug.assert(@sizeOf(GdtEntryExtended) == 16);

    std.debug.assert(@sizeOf(DescriptorTableRegister) == 10);
    std.debug.assert(@bitOffsetOf(DescriptorTableRegister, "limit") == 0);
    std.debug.assert(@bitOffsetOf(DescriptorTableRegister, "base") == 16);
}
