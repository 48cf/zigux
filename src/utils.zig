const logger = std.log.scoped(.utils);

const std = @import("std");

pub fn kib(comptime value: comptime_int) comptime_int {
    return value * 1024;
}

pub fn mib(comptime value: comptime_int) comptime_int {
    return kib(value) * 1024;
}

pub fn gib(comptime value: comptime_int) comptime_int {
    return mib(value) * 1024;
}

pub fn tib(comptime value: comptime_int) comptime_int {
    return gib(value) * 1024;
}

pub fn alignDown(comptime T: type, value: T, comptime alignment: T) T {
    return value - (value % alignment);
}

pub fn alignUp(comptime T: type, value: T, comptime alignment: T) T {
    return alignDown(T, value + alignment - 1, alignment);
}

pub fn isAligned(comptime T: type, value: T, comptime alignment: T) bool {
    return alignDown(T, value, alignment) == value;
}

pub fn vital(value: anytype, comptime message: []const u8) @TypeOf(value catch unreachable) {
    return value catch |err| {
        logger.err("A vital check failed: {s}", .{@errorName(err)});

        std.debug.panicExtra(@errorReturnTrace(), message, .{});
    };
}
