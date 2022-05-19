const std = @import("std");

export fn _start() callconv(.Naked) noreturn {
    asm volatile (
        \\mov (%%rsp), %%rdi
        \\lea 0x8(%%rsp), %%rsi
        \\lea 0x10(%%rsp, %%rdi, 0x08), %%rsi
        \\call main
        \\mov %%rax, %%rdi
        \\mov $0x3C, %%rax
        \\syscall
    );

    unreachable;
}

export fn main(
    argc: i32,
    argv: [*:null]const ?[*:0]const u8,
    envp: [*:null]const ?[*:0]const u8,
) callconv(.C) i32 {
    std.log.info("Argument count = {}", .{argc});

    for (std.mem.span(argv)) |arg| {
        std.log.info("- {s}", .{std.mem.span(arg)});
    }

    std.log.info("Environment variables:", .{});

    for (std.mem.span(envp)) |env| {
        std.log.info("- {s}", .{std.mem.span(env)});
    }

    return 69;
}

pub fn log(
    comptime level: std.log.Level,
    comptime scope: anytype,
    comptime fmt: []const u8,
    args: anytype,
) void {
    _ = level;
    _ = scope;

    var bytes: [1024]u8 = undefined;
    var buffer = std.io.fixedBufferStream(&bytes);
    var writer = buffer.writer();

    writer.print(fmt ++ "\n", args) catch unreachable;
    writer.writeByte(0) catch unreachable;

    _ = std.os.linux.write(
        1,
        &bytes,
        std.mem.len(@ptrCast([*:0]const u8, &bytes)),
    );
}
