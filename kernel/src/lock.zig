const std = @import("std");

const arch = @import("arch.zig");
const debug = @import("debug.zig");

pub const CriticalSection = struct {
    previous_rflags: u64,

    pub fn enter() @This() {
        const rflags = arch.readEFlags();
        asm volatile ("cli");
        return .{ .previous_rflags = rflags };
    }

    pub fn leave(self: @This()) void {
        if ((self.previous_rflags & (1 << 9)) != 0) {
            asm volatile ("sti");
        }
    }
};

pub const Spinlock = struct {
    value: std.atomic.Value(bool) = .{ .raw = false },

    pub fn tryLock(self: *@This()) bool {
        return self.value.cmpxchgStrong(false, true, .acquire, .monotonic) == null;
    }

    pub fn lock(self: *@This()) void {
        while (!self.tryLock()) {
            std.atomic.spinLoopHint();
        }
    }

    pub fn unlock(self: *@This()) void {
        self.value.store(false, .release);
    }
};
