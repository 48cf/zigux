const logger = std.log.scoped(.ps2);

const std = @import("std");

const arch = @import("../arch.zig");
const apic = @import("../apic.zig");
const interrupts = @import("../interrupts.zig");
const scheduler = @import("../scheduler.zig");

var keyboard_semaphore = scheduler.Semaphore.init(0);

fn keyboardHandler(frame: *interrupts.InterruptFrame) void {
    _ = frame;
    _ = arch.in(u8, 0x60);

    keyboard_semaphore.release(1);

    apic.eoi();
}

fn keyboardThread() noreturn {
    logger.info("Started the PS2 thread", .{});

    while (true) {
        keyboard_semaphore.acquire(1);

        logger.debug("A PS2 keyboard interrupt arrived", .{});
    }
}

pub fn init() void {
    const keyboard_vector = interrupts.allocateVector();

    interrupts.registerHandler(keyboard_vector, keyboardHandler);

    _ = scheduler.startKernelThread(keyboardThread) catch unreachable;

    apic.routeIrq(1, apic.localApicId(), keyboard_vector);

    _ = arch.in(u8, 0x60);
}
