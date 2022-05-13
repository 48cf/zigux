const logger = std.log.scoped(.debug);

const std = @import("std");

fn printSymbol(address: u64) void {
    logger.err("  0x{X:0>16}", .{address});
}

pub fn printStackIterator(stack_iter: std.debug.StackIterator) void {
    var iter = stack_iter;

    logger.err("Stack backtrace:", .{});

    if (iter.first_address) |addr| {
        printSymbol(addr);
    }

    while (iter.next()) |addr| {
        printSymbol(addr);
    }
}

pub fn printStackTrace(stack_trace: *std.builtin.StackTrace) void {
    logger.err("Stack backtrace:", .{});

    var frame_index: usize = 0;
    var frames_left: usize = std.math.min(stack_trace.index, stack_trace.instruction_addresses.len);

    while (frames_left != 0) : ({
        frames_left -= 1;
        frame_index = (frame_index + 1) % stack_trace.instruction_addresses.len;
    }) {
        const return_address = stack_trace.instruction_addresses[frame_index];

        printSymbol(return_address);
    }
}
