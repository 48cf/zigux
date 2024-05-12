const logger = std.log.scoped(.hpet);

const std = @import("std");

const acpi = @import("./acpi.zig");
const scheduler = @import("./scheduler.zig");
const uacpi = @import("./uacpi.zig");
const virt = @import("./virt.zig");

var hpet_address: ?*anyopaque = null;
var counter_period: u64 = 0;

const Register = enum(usize) {
    capabilities = 0x0,
    configuration = 0x10,
    main_counter = 0xF0,
};

fn getRegister(reg: Register) *volatile u64 {
    return @ptrFromInt(@intFromPtr(hpet_address.?) + @intFromEnum(reg));
}

fn ensureIsSane() bool {
    if (getRegister(.capabilities).* & (1 << 13) == 0) {
        logger.err("HPET main counter is not 64-bits wide", .{});
        return false;
    }

    return true;
}

fn tryInit() bool {
    const hpet_table = acpi.findTable(uacpi.ACPI_HPET_SIGNATURE) orelse {
        logger.err("HPET table not found", .{});
        return false;
    };

    const hpet: *uacpi.acpi_hpet = @ptrCast(hpet_table.uacpi_table.unnamed_0.hdr);
    if (hpet.address.address_space_id != uacpi.UACPI_ADDRESS_SPACE_SYSTEM_MEMORY) {
        logger.err("HPET address space is not system memory", .{});
        return false;
    }

    hpet_address = virt.asHigherHalfUncached(*anyopaque, hpet.address.address);
    logger.debug("HPET base address is 0x{X}", .{hpet.address.address});

    if (!ensureIsSane()) {
        hpet_address = null;
        return false;
    }

    return true;
}

fn finishInit() void {
    counter_period = getRegister(.capabilities).* >> 32;
    logger.info("HPET counter period is {d}fs", .{counter_period});

    getRegister(.configuration).* &= ~@as(u64, 1 << 0);
    getRegister(.main_counter).* = 0;
    getRegister(.configuration).* |= (1 << 0);
}

pub fn init(force_init: bool) !void {
    defer {
        if (hpet_address != null) {
            finishInit();
        }
    }

    if (tryInit()) {
        return;
    }

    if (force_init) {
        logger.warn("Could not find an HPET table, assuming the memory location", .{});
        hpet_address = virt.asHigherHalfUncached(*anyopaque, 0xFED00000);

        if (ensureIsSane()) {
            return;
        }
    }

    hpet_address = null;
    return error.Unsupported;
}

pub fn sample() u64 {
    return getRegister(.main_counter).*;
}

pub fn sleep(ns: u64, preempt: bool) void {
    const start = sample();
    const end = start + (ns * 1000000) / counter_period;

    if (preempt) {
        while (sample() < end) {
            scheduler.yield();
        }
    } else {
        while (sample() < end) {
            std.atomic.spinLoopHint();
        }
    }
}
