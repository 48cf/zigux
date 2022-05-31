const std = @import("std");

pub const State = enum(i32) {
    Unlocked,
    Locked,
    Waiting,
};

pub const AtomicMutex = struct {
    state: State = .Unlocked,

    pub fn tryLock(self: *AtomicMutex) bool {
        return @cmpxchgStrong(State, &self.state, .Unlocked, .Locked, .Acquire, .Monotonic) == null;
    }

    pub fn lock(self: *AtomicMutex) void {
        switch (@atomicRmw(State, &self.state, .Xchg, .Locked, .Acquire)) {
            .Unlocked => {},
            else => |state| self.lockSlow(state),
        }
    }

    pub fn unlock(self: *AtomicMutex) void {
        switch (@atomicRmw(State, &self.state, .Xchg, .Unlocked, .Release)) {
            .Unlocked => unreachable,
            .Waiting => unreachable,
            .Locked => {},
        }
    }

    fn lockSlow(self: *AtomicMutex, curr_state: State) void {
        var new_state = curr_state;
        var spin: u8 = 0;

        while (spin < 100) : (spin += 1) {
            const state = @cmpxchgWeak(State, &self.state, .Unlocked, new_state, .Acquire, .Monotonic) orelse return;

            switch (state) {
                .Unlocked => {},
                .Locked => {},
                .Waiting => break,
            }

            var iter = std.math.min(32, spin + 1);

            while (iter > 0) : (iter -= 1) {
                std.atomic.spinLoopHint();
            }
        }

        new_state = .Waiting;

        while (true) {
            switch (@atomicRmw(State, &self.state, .Xchg, new_state, .Acquire)) {
                .Unlocked => return,
                else => {},
            }

            std.atomic.spinLoopHint();
        }
    }
};
