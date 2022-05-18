const logger = std.log.scoped(.apic);

const std = @import("std");

const interrupts = @import("interrupts.zig");
const per_cpu = @import("per_cpu.zig");
const scheduler = @import("scheduler.zig");

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

var timer_vector: ?u8 = null;
var one_shot_vector: ?u8 = null;

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
    writeRegister(.LvtInitialCount, 0x200000);
}

pub fn eoi() void {
    writeRegister(.Eoi, 0);
}

fn readRegister(reg: ApicRegister) u32 {
    const address = per_cpu.get().lapic_base + @enumToInt(reg);
    const pointer = @intToPtr(*volatile u32, address);

    return pointer.*;
}

fn writeRegister(reg: ApicRegister, value: u32) void {
    const address = per_cpu.get().lapic_base + @enumToInt(reg);
    const pointer = @intToPtr(*volatile u32, address);

    pointer.* = value;
}
