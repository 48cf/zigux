const std = @import("std");

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
