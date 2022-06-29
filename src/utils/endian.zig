const std = @import("std");

pub fn Wrapper(comptime T: type, comptime endianess: std.builtin.Endian) type {
    return extern struct {
        inner: T,

        const This = @This();

        pub fn read(self: *const This) T {
            return std.mem.toNative(T, self.inner, endianess);
        }

        pub fn write(self: *This, value: T) void {
            self.inner = std.mem.nativeTo(T, value, endianess);
        }
    };
}

pub fn Big(comptime T: type) type {
    return Wrapper(T, .Big);
}

pub fn Little(comptime T: type) type {
    return Wrapper(T, .Little);
}
