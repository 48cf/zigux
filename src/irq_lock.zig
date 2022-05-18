const std = @import("std");

const arch = @import("arch.zig");
const debug = @import("debug.zig");

pub const IrqSpinlock = struct {
    inner: std.Thread.Mutex.AtomicMutex = .{},
    re_enable: bool = undefined,

    pub fn lock(self: *IrqSpinlock) bool {
        if (arch.readEflags() & 0x200 != 0) {
            while (true) {
                asm volatile ("cli");

                if (!self.inner.tryLock()) {
                    asm volatile ("sti");
                    continue;
                } else {
                    self.re_enable = true;
                    break;
                }
            }
        } else {
            self.inner.lock();
            self.re_enable = false;
        }

        return self.re_enable;
    }

    pub fn ungrab(self: *IrqSpinlock) void {
        self.inner.unlock();
    }

    pub fn unlock(self: *IrqSpinlock) void {
        const re_enable = self.re_enable;

        self.ungrab();

        if (re_enable) {
            asm volatile ("sti");
        }
    }
};
