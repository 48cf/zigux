const logger = std.log.scoped(.apic);

const std = @import("std");

const interrupts = @import("interrupts.zig");
const per_cpu = @import("per_cpu.zig");
const scheduler = @import("scheduler.zig");
const virt = @import("virt.zig");

const ApicRegister = enum(usize) {
    CpuId = 0x20,
    Eoi = 0xB0,
    SpuriousVector = 0xF0,
    Icr0 = 0x300,
    Icr1 = 0x310,
    LvtInitialCount = 0x380,
    LvtCurrentCount = 0x390,
    LvtTimer = 0x320,
    LvtDivide = 0x3E0,
};

const IoApic = struct {
    address: u32,
    base_gsi: u32,

    fn read(self: *const IoApic, offset: u32) u32 {
        @intToPtr(*volatile u32, virt.asHigherHalf(self.address + 0x00)).* = offset;
        return @intToPtr(*volatile u32, virt.asHigherHalf(self.address + 0x10)).*;
    }

    fn write(self: *const IoApic, offset: u32, value: u32) void {
        @intToPtr(*volatile u32, virt.asHigherHalf(self.address + 0x00)).* = offset;
        @intToPtr(*volatile u32, virt.asHigherHalf(self.address + 0x10)).* = value;
    }

    fn gsi_count(self: *const IoApic) u32 {
        return (self.read(1) >> 16) & 0xFF;
    }

    fn route(self: *const IoApic, gsi: u32, lapic_id: u32, vector: u8, flags: u16) void {
        const value = @intCast(u64, vector) | @intCast(u64, flags & 0b1010) << 12 | @intCast(u64, lapic_id);
        const offset = 0x10 + (gsi - self.base_gsi) * 2;

        self.write(offset + 0, @truncate(u32, value));
        self.write(offset + 1, @truncate(u32, value >> 32));
    }
};

const SourceOverride = struct {
    ioapic_id: u8,
    gsi: u32,
    flags: u16,
};

var timer_vector: ?u8 = null;
var one_shot_vector: ?u8 = null;
var io_apics = [1]?IoApic{null} ** 16;
var source_overrides = [1]?SourceOverride{null} ** 256;

fn timerHandler(frame: *interrupts.InterruptFrame) void {
    scheduler.reschedule(frame);

    eoi();
}

// TODO: Implement one shot timers for scheduling
fn oneShotHandler(frame: *interrupts.InterruptFrame) void {
    _ = frame;

    eoi();
}

pub fn init() void {
    writeRegister(.SpuriousVector, @as(u32, interrupts.spurious_vector) | 0x100);
}

pub fn initTimer() void {
    if (timer_vector == null) {
        timer_vector = interrupts.allocateVector();
        one_shot_vector = interrupts.allocateVector();
    }

    interrupts.registerHandler(timer_vector.?, timerHandler);
    interrupts.registerHandler(one_shot_vector.?, oneShotHandler);

    // ░░░░░▄▄▄▄▀▀▀▀▀▀▀▀▄▄▄▄▄▄░░░░░░░░
    // ░░░░░█░░░░▒▒▒▒▒▒▒▒▒▒▒▒░░▀▀▄░░░░
    // ░░░░█░░░▒▒▒▒▒▒░░░░░░░░▒▒▒░░█░░░
    // ░░░█░░░░░░▄██▀▄▄░░░░░▄▄▄░░░░█░░
    // ░▄▀▒▄▄▄▒░█▀▀▀▀▄▄█░░░██▄▄█░░░░█░
    // █░▒█▒▄░▀▄▄▄▀░░░░░░░░█░░░▒▒▒▒▒░█
    // █░▒█░█▀▄▄░░░░░█▀░░░░▀▄░░▄▀▀▀▄▒█
    // ░█░▀▄░█▄░█▀▄▄░▀░▀▀░▄▄▀░░░░█░░█░
    // ░░█░░░▀▄▀█▄▄░█▀▀▀▄▄▄▄▀▀█▀██░█░░
    // ░░░█░░░░██░░▀█▄▄▄█▄▄█▄████░█░░░
    // ░░░░█░░░░▀▀▄░█░░░█░█▀██████░█░░
    // ░░░░░▀▄░░░░░▀▀▄▄▄█▄█▄█▄█▄▀░░█░░
    // ░░░░░░░▀▄▄░▒▒▒▒░░░░░░░░░░▒░░░█░
    // ░░░░░░░░░░▀▀▄▄░▒▒▒▒▒▒▒▒▒▒░░░░█░
    // ░░░░░░░░░░░░░░▀▄▄▄▄▄░░░░░░░░█░░
    // ░░░░░░░░░░░░░░░░░░░░▀▀▀▀▀▀▀▀░░░

    writeRegister(.LvtDivide, 3);
    writeRegister(.LvtTimer, @as(u32, timer_vector.?) | 0x20000);
    writeRegister(.LvtInitialCount, 0x100000);
}

pub fn eoi() void {
    writeRegister(.Eoi, 0);
}

pub fn localApicId() u32 {
    return readRegister(.CpuId);
}

pub fn routeIrq(irq: u8, lapic_id: u32, vector: u8) void {
    const gsi = mapIrqToGsi(irq);
    const ioapic_id = mapGsiToIoApic(gsi.gsi);
    const ioapic = &io_apics[ioapic_id].?;

    ioapic.route(gsi.gsi, lapic_id, vector, gsi.flags);
}

pub fn handleIoApic(id: u8, address: u32, base_gsi: u32) void {
    std.debug.assert(io_apics[id] == null);

    io_apics[id] = .{
        .address = address,
        .base_gsi = base_gsi,
    };
}

pub fn handleIoApicIso(ioapic_id: u8, irq_source: u8, gsi: u32, flags: u16) void {
    std.debug.assert(source_overrides[irq_source] == null);

    source_overrides[irq_source] = .{
        .ioapic_id = ioapic_id,
        .gsi = gsi,
        .flags = flags,
    };
}

fn mapGsiToIoApic(gsi: u32) u8 {
    for (io_apics) |io_apic, i| {
        if (io_apic) |ioa| {
            const gsi_count = ioa.gsi_count();

            if (gsi >= ioa.base_gsi and gsi < ioa.base_gsi + gsi_count)
                return @intCast(u8, i);
        }
    }

    std.debug.panic("Could not find an IOAPIC for GSI {}", .{gsi});
}

fn mapIrqToGsi(irq: u8) SourceOverride {
    return source_overrides[irq] orelse .{
        .ioapic_id = mapGsiToIoApic(irq),
        .gsi = @as(u32, irq),
        .flags = 0,
    };
}

fn readRegister(reg: ApicRegister) u32 {
    const address = per_cpu.get().lapic_base + @enumToInt(reg);
    const pointer = @intToPtr(*align(4) volatile u32, address);

    return pointer.*;
}

fn writeRegister(reg: ApicRegister, value: u32) void {
    const address = per_cpu.get().lapic_base + @enumToInt(reg);
    const pointer = @intToPtr(*align(4) volatile u32, address);

    pointer.* = value;
}
