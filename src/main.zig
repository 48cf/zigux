const logger = std.log.scoped(.fw_main);

const std = @import("std");
const dtb = @import("utils/dtb.zig");
const utils = @import("utils/utils.zig");
const phys = @import("memory/phys.zig");

pub const log_level = std.log.Level.debug;

const uart_base: u64 = 0x9000000;
const dram_base: u64 = 0x40000000;
const relocate_base: u64 = dram_base + 0x100000;

export fn fwMain(relocate_end: u64) noreturn {
    logger.info("Hello, world!", .{});

    const dtb_bytes = @alignCast(4, @intToPtr([*]u8, dram_base));
    const fdt = dtb.parse(dtb_bytes) catch unreachable;
    const memory_node = fdt.find("memory@", "reg") orelse @panic("Could not find DRAM DTB node");
    const dram_size = std.mem.readIntBig(u64, memory_node[8..16]);
    const image_size = utils.alignUp(u64, relocate_end - relocate_base, 0x1000);

    phys.initialize(dram_base, dram_size, relocate_base, image_size);

    while (true) {
        asm volatile ("wfi");
    }
}

export fn _start() linksection(".text.entry") callconv(.Naked) noreturn {
    const ibrd_value: u32 = 0x10;
    const cr_value: u32 = 0xC301;

    // Initialize the UART controller
    asm volatile (
        \\str w0, [%[uart_base], #0x24]
        \\str w1, [%[uart_base], #0x30]
        :
        : [uart_base] "r" (uart_base),
          [ibrd_value] "{w0}" (ibrd_value),
          [cr_value] "{w1}" (cr_value),
    );

    // Relocate into R/W memory
    asm volatile (
        \\  adr x1, __blob_begin
        \\  adr x2, __blob_end
        \\  mov x0, %[relocate_base]
        \\  add x5, x0, copy_done - _start
        \\  mov sp, x0
        \\copy_loop:
        \\  ldp x3, x4, [x1], #0x10
        \\  stp x3, x4, [x0], #0x10
        \\  cmp x1, x2
        \\  b.lt copy_loop
        \\  br x5
        \\copy_done:
        \\  b fwMain
        :
        : [relocate_base] "i" (relocate_base),
    );

    unreachable;
}

pub fn log(
    comptime level: std.log.Level,
    comptime scope: anytype,
    comptime fmt: []const u8,
    args: anytype,
) void {
    const LogBuffer = struct {
        var buffer: [16 * 4096]u8 = undefined;
    };

    var buffer = std.io.fixedBufferStream(&LogBuffer.buffer);
    var writer = buffer.writer();

    writer.print("{s}({s}): ", .{ @tagName(level), @tagName(scope) }) catch unreachable;
    writer.print(fmt ++ "\n", args) catch unreachable;

    for (buffer.getWritten()) |ch| {
        @intToPtr(*volatile u32, uart_base).* = ch;
    }
}
