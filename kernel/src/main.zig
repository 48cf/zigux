const std = @import("std");
const limine = @import("limine");

const arch = @import("arch.zig");
const pmm = @import("pmm.zig");

fn debug_write(_: void, bytes: []const u8) !usize {
    arch.debug_print(bytes);
    return bytes.len;
}

const DebugWriter = std.io.Writer(void, error{}, debug_write);

pub const std_options = struct {
    pub fn logFn(
        comptime level: std.log.Level,
        comptime scope: anytype,
        comptime fmt: []const u8,
        args: anytype,
    ) void {
        const level_txt = comptime level.asText();
        const prefix2 = if (scope == .default) ": " else "(" ++ @tagName(scope) ++ "): ";

        var writer: DebugWriter = undefined;
        std.fmt.format(writer, level_txt ++ prefix2 ++ fmt ++ "\n", args) catch return;
    }
};

pub export var boot_info_req: limine.BootloaderInfoRequest = .{};

pub var bsp_cpu: arch.Cpu = undefined;

export fn _start() callconv(.C) noreturn {
    const boot_info_res = boot_info_req.response.?;

    std.log.info("Booted using {s} {s}", .{ boot_info_res.name, boot_info_res.version });

    arch.init_cpu(&bsp_cpu);

    pmm.init();

    std.log.info("Halting...", .{});

    while (true) {}
}

pub fn panic(msg: []const u8, _: ?*std.builtin.StackTrace, _: ?usize) noreturn {
    std.log.err("Kernel panic: {s}", .{msg});

    var stack_iter = std.debug.StackIterator.init(@returnAddress(), @frameAddress());

    while (stack_iter.next()) |addr| {
        std.log.err(" - 0x{x:0>16}", .{addr});
    }

    while (true) {}
}
