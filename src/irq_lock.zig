const std = @import("std");

const arch = @import("arch.zig");
const debug = @import("debug.zig");

pub const IrqSpinlock = struct {
    inner: std.Thread.Mutex.AtomicMutex = .{},
    re_enable: bool = undefined,

    pub fn lock(self: *IrqSpinlock) void {
        if (arch.readEflags() & 0x200 != 0) {
            while (true) {
                asm volatile ("cli");

                if (!self.inner.tryLock()) {
                    asm volatile ("sti");
                    continue;
                } else {
                    self.re_enable = true;
                    return;
                }
            }
        } else {
            self.inner.lock();
            self.re_enable = false;
        }
    }

    pub fn unlock(self: *IrqSpinlock) void {
        const re_enable = self.re_enable;

        self.inner.unlock();

        if (re_enable) {
            asm volatile ("sti");
        }
    }
};
