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

fn timer_handler(frame: *interrupts.InterruptFrame) void {
    scheduler.reschedule(frame);

    eoi();
}

fn one_shot_handler(frame: *interrupts.InterruptFrame) void {
    _ = frame;

    eoi();
}

pub fn init() void {
    write_reg(.SpuriousVector, @as(u32, interrupts.spurious_vector) | 0x100);
}

pub fn init_timer() void {
    if (timer_vector == null) {
        timer_vector = interrupts.allocate_vector();
        one_shot_vector = interrupts.allocate_vector();
    }

    interrupts.register_handler(timer_vector.?, timer_handler);
    interrupts.register_handler(one_shot_vector.?, one_shot_handler);

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

    write_reg(.LvtDivide, 3);
    write_reg(.LvtTimer, @as(u32, timer_vector.?) | 0x20000);
    write_reg(.LvtInitialCount, 0x200000);
}

pub fn eoi() void {
    write_reg(.Eoi, 0);
}

fn read_reg(reg: ApicRegister) u32 {
    const address = per_cpu.get().lapic_base + @enumToInt(reg);
    const pointer = @intToPtr(*volatile u32, address);

    return pointer.*;
}

fn write_reg(reg: ApicRegister, value: u32) void {
    const address = per_cpu.get().lapic_base + @enumToInt(reg);
    const pointer = @intToPtr(*volatile u32, address);

    pointer.* = value;
}
