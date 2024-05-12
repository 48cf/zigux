const logger = std.log.scoped(.apic);

const std = @import("std");

const hpet = @import("./hpet.zig");
const interrupts = @import("./interrupts.zig");
const per_cpu = @import("./per_cpu.zig");
const scheduler = @import("./scheduler.zig");
const time = @import("./time.zig");
const uacpi = @import("./uacpi.zig");
const virt = @import("./virt.zig");

const APICRegister = enum(usize) {
    lapic_id = 0x20,
    eoi = 0xB0,
    spurious_vector = 0xF0,
    icr0 = 0x300,
    icr1 = 0x310,
    lvt_timer = 0x320,
    timer_initial_count = 0x380,
    timer_current_count = 0x390,
    timer_divide = 0x3E0,
};

fn getLAPICRegister(reg: APICRegister) *volatile u32 {
    return @as(*volatile u32, @ptrFromInt(per_cpu.get().lapic_base + @intFromEnum(reg)));
}

const IOAPIC = struct {
    ioregsel: *volatile u32,
    iowin: *volatile u32,

    base_gsi: u32,
    gsi_count: u32,

    fn read(self: *const @This(), offset: u32) u32 {
        self.ioregsel.* = offset;
        return self.iowin.*;
    }

    fn write(self: *const @This(), offset: u32, value: u32) void {
        self.ioregsel.* = offset;
        self.iowin.* = value;
    }

    const RouteFlags = struct {
        delivery_mode: enum { fixed, lowest_priority, smi, nmi, init, ext_int },
        pin_polarity: enum { high, low },
        trigger_mode: enum { edge, level },
        masked: bool,
    };

    fn route(self: *const @This(), gsi: u32, lapic_id: u32, vector: u8, flags: RouteFlags) void {
        const delivery_mode: u3 = switch (flags.delivery_mode) {
            .fixed => 0b000,
            .lowest_priority => 0b001,
            .smi => 0b010,
            .nmi => 0b100,
            .init => 0b101,
            .ext_int => 0b111,
        };

        const offset = 0x10 + (gsi - self.base_gsi) * 2;
        const entry: u64 = vector |
            @as(u64, delivery_mode) << 8 |
            @as(u64, @intFromBool(flags.pin_polarity == .low)) << 13 |
            @as(u64, @intFromBool(flags.trigger_mode == .level)) << 15 |
            @as(u64, @intFromBool(flags.masked)) << 16 |
            @as(u64, @intCast(lapic_id)) << 56;

        self.write(offset, @truncate(entry));
        self.write(offset + 1, @intCast(entry >> 32));
    }
};

const InterruptSourceOverride = struct {
    bus: u8,
    gsi: u8,
    flags: u16,
};

var timer_ticks_per_ms: u64 = 0;
var ioapics: std.BoundedArray(IOAPIC, 16) = .{};
var isos = [1]?InterruptSourceOverride{null} ** 16;

fn timerHandler(frame: *interrupts.InterruptFrame) void {
    const increment: time.Timespec = .{ .seconds = 0, .nanoseconds = std.time.ns_per_ms };
    time.setClock(.monotonic, time.getClock(.monotonic).add(increment));
    time.setClock(.realtime, time.getClock(.realtime).add(increment));
    scheduler.reschedule(frame);
    eoi();
}

pub fn init() void {
    getLAPICRegister(.spurious_vector).* = @as(u32, 0xFF) | 0x100;

    // Calibrate the LAPIC timer using the HPET
    getLAPICRegister(.lvt_timer).* = (1 << 16); // Masked, one shot
    getLAPICRegister(.timer_divide).* = 0b0001; // Divide by 4

    const initial_count = 0x1000_0000;
    getLAPICRegister(.timer_initial_count).* = initial_count;

    hpet.sleep(std.time.ns_per_ms * 10, false);
    const count = getLAPICRegister(.timer_current_count).*;
    std.debug.assert(count != 0);

    timer_ticks_per_ms = (initial_count - count) / 10;
    logger.info("LAPIC timer ticks at {d} ticks/ms", .{timer_ticks_per_ms});

    const timer_vector = interrupts.allocateVector();
    interrupts.registerHandler(timer_vector, timerHandler);

    getLAPICRegister(.lvt_timer).* = @as(u32, timer_vector) | (1 << 17); // Periodic mode
    getLAPICRegister(.timer_initial_count).* = @intCast(timer_ticks_per_ms);
}

pub fn eoi() void {
    getLAPICRegister(.eoi).* = 0;
}

pub fn getLapicID() u32 {
    return getLAPICRegister(.lapic_id).* >> 24;
}

pub fn routeISAIRQ(irq: u8, lapic_id: u32, vector: u8, masked: bool) bool {
    var gsi = irq;
    var route_flags: IOAPIC.RouteFlags = .{
        .delivery_mode = .fixed,
        .pin_polarity = .high,
        .trigger_mode = .edge,
        .masked = masked,
    };

    if (isos[irq]) |iso| {
        gsi = @intCast(iso.gsi);
        switch (iso.flags & uacpi.ACPI_MADT_POLARITY_MASK) {
            uacpi.ACPI_MADT_POLARITY_ACTIVE_LOW => route_flags.pin_polarity = .low,
            uacpi.ACPI_MADT_POLARITY_ACTIVE_HIGH => route_flags.pin_polarity = .high,
            else => {},
        }
        switch (iso.flags & uacpi.ACPI_MADT_TRIGGERING_MASK) {
            uacpi.ACPI_MADT_TRIGGERING_EDGE => route_flags.trigger_mode = .edge,
            uacpi.ACPI_MADT_TRIGGERING_LEVEL => route_flags.trigger_mode = .level,
            else => {},
        }
    }

    var responsible_ioapic: ?*const IOAPIC = null;
    for (ioapics.constSlice()) |*it| {
        if (it.base_gsi <= irq and it.base_gsi + it.gsi_count > irq) {
            responsible_ioapic = it;
            break;
        }
    }

    if (responsible_ioapic) |ioapic| {
        ioapic.route(gsi, lapic_id, vector, route_flags);
        return true;
    }

    logger.warn("Failed to route ISA IRQ {d}, could not find responsible IOAPIC", .{irq});
    return false;
}

pub fn handleIOAPIC(address: u32, base_gsi: u32) void {
    var ioapic: IOAPIC = .{
        .ioregsel = @ptrFromInt(address),
        .iowin = @ptrFromInt(address + 0x10),
        .base_gsi = base_gsi,
        .gsi_count = undefined,
    };

    ioapic.gsi_count = (ioapic.read(0x1) >> 16) & 0xFF;
    ioapics.append(ioapic) catch @panic("Exceeded maximum number of IOAPICs");
}

pub fn handleIOAPICISO(bus: u8, irq: u8, gsi: u32, flags: u16) void {
    std.debug.assert(isos[irq] == null);
    isos[irq] = .{ .bus = bus, .gsi = @intCast(gsi), .flags = flags };
}
