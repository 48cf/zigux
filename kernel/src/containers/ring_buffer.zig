// https://github.com/FlorenceOS/Florence/blob/master/lib/containers/ring_buffer.zig

const scheduler = @import("../scheduler.zig");

pub fn RingBuffer(comptime T: type, comptime max_size: usize) type {
    if (@popCount(max_size) != 1) {
        // Required both for mask() and `written`/`read` overflowing
        @compileError("Size must be a power of 2!");
    }

    return struct {
        written: usize = 0,
        read: usize = 0,

        elements: [max_size]T = undefined,

        fn mask(value: usize) usize {
            return value & (max_size - 1);
        }

        pub fn size(self: @This()) usize {
            return self.written - self.read;
        }

        pub fn empty(self: @This()) bool {
            return self.size() == 0;
        }

        pub fn full(self: @This()) bool {
            return self.size() == max_size;
        }

        // Peek at the next writable slot, call send() when done writing to
        // If you don't want to write, no further action is needed
        pub fn peekWrite(self: *@This()) ?*T {
            if (self.full()) {
                return null;
            } else {
                return &self.elements[mask(self.written)];
            }
        }

        pub fn send(self: *@This()) void {
            // Overflowing here is fine, max size is power of 2
            self.written +%= 1;
        }

        pub fn push(self: *@This(), value: T) bool {
            if (self.peekWrite()) |elem| {
                elem.* = value;
                self.send();
                return true;
            } else {
                return false;
            }
        }

        pub fn peek(self: *@This()) ?*T {
            if (self.empty()) {
                return null;
            } else {
                return &self.elements[mask(self.read)];
            }
        }

        pub fn drop(self: *@This()) void {
            // Overflowing here is fine, max size is power of 2
            self.read +%= 1;
        }

        pub fn pop(self: *@This()) ?T {
            if (self.peek()) |value| {
                const v = value.*;
                self.drop();
                return v;
            } else {
                return null;
            }
        }
    };
}

// A RingBuffer that you can wait on and that also tracks the number of dropped elements in case the buffer is full when pushed to.
// Pushing can be done from an interrupt context, but popping cannot.
pub fn RingWaitQueue(comptime T: type, comptime max_size: usize) type {
    return struct {
        num_dropped: usize = 0,
        semaphore: scheduler.Semaphore = scheduler.Semaphore.init(0),
        buffer: RingBuffer(T, max_size) = .{},

        pub fn push(self: *@This(), value: T) bool {
            if (self.buffer.push(value)) {
                self.semaphore.release(1);
                return true;
            } else {
                _ = @atomicRmw(usize, &self.num_dropped, .Add, 1, .acq_rel);
                return false;
            }
        }

        pub fn get(self: *@This()) T {
            while (true) {
                if (self.buffer.pop()) |p| {
                    return p;
                }

                self.semaphore.acquire(1);
            }
        }

        // Set the number of dropped elements to 0 and return old value
        pub fn dropped(self: *@This()) usize {
            return @atomicRmw(usize, &self.num_dropped, .Xchg, 0, .acq_rel);
        }
    };
}
