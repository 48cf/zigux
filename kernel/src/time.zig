const logger = std.log.scoped(.time);

const std = @import("std");
const limine = @import("limine");

const Clock = enum {
    realtime,
    monotonic,
};

pub const Timespec = struct {
    seconds: i64,
    nanoseconds: i64,

    pub fn add(self: @This(), other: @This()) @This() {
        var result = self;
        if (self.nanoseconds + other.nanoseconds >= std.time.ns_per_s - 1) {
            result.nanoseconds = (self.nanoseconds + other.nanoseconds) - std.time.ns_per_s;
            result.seconds += 1;
        } else {
            result.nanoseconds += other.nanoseconds;
        }
        result.seconds += other.seconds;
        return result;
    }
};

var realtime_clock: Timespec = .{ .seconds = 0, .nanoseconds = 0 };
var monotonic_clock: Timespec = .{ .seconds = 0, .nanoseconds = 0 };

pub fn init(time_res: *limine.BootTimeResponse) void {
    realtime_clock.seconds = time_res.boot_time;
}

pub fn getClock(clock: Clock) Timespec {
    return switch (clock) {
        .realtime => realtime_clock,
        .monotonic => monotonic_clock,
    };
}

pub fn setClock(clock: Clock, time: Timespec) void {
    switch (clock) {
        .realtime => realtime_clock = time,
        .monotonic => monotonic_clock = time,
    }
}
