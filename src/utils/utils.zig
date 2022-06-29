pub inline fn kib(comptime value: comptime_int) comptime_int {
    return value * 1024;
}

pub inline fn mib(comptime value: comptime_int) comptime_int {
    return kib(value) * 1024;
}

pub inline fn gib(comptime value: comptime_int) comptime_int {
    return mib(value) * 1024;
}

pub inline fn tib(comptime value: comptime_int) comptime_int {
    return gib(value) * 1024;
}

pub inline fn alignDown(comptime T: type, value: T, alignment: T) T {
    return value - (value % alignment);
}

pub inline fn alignUp(comptime T: type, value: T, alignment: T) T {
    return alignDown(T, value + alignment - 1, alignment);
}

pub inline fn divRoundUp(comptime T: type, value: T, alignment: T) T {
    return (value + (alignment - 1)) / alignment;
}

pub inline fn isAligned(comptime T: type, value: T, alignment: T) bool {
    return alignDown(T, value, alignment) == value;
}

pub fn range(length: usize) []const void {
    return @as([*]const void, undefined)[0..length];
}
