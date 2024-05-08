const logger = std.log.scoped(.ps2);

const root = @import("root");
const std = @import("std");

const arch = @import("../arch.zig");
const apic = @import("../apic.zig");
const acpi = @import("../acpi.zig");
const interrupts = @import("../interrupts.zig");
const scheduler = @import("../scheduler.zig");
const input = @import("./input.zig");

var keyboard_buffer: std.BoundedArray(u8, 8) = .{};

fn finishSequence(offset: usize, sequence: []const u8) bool {
    const buffer = keyboard_buffer.slice()[offset..];

    if (buffer.len >= sequence.len) {
        if (std.mem.eql(u8, buffer, sequence)) {
            keyboard_buffer.resize(0) catch unreachable;
            return true;
        } else {
            std.debug.panic("Unexpected scancode sequence: {any}, expected: {any}", .{ buffer, sequence });
        }
    }

    return false;
}

fn keyLocation(extended: bool, scan_code: u8) ?input.KeyLocation {
    if (extended) {
        return switch (scan_code) {
            0x10 => .MediaRewind,
            0x19 => .MediaForward,
            0x1C => .NumpadEnter,
            0x1D => .RightCtrl,
            0x20 => .MediaMute,
            0x22 => .MediaPausePlay,
            0x24 => .MediaStop,
            0x2E => .MediaVolumeDown,
            0x30 => .MediaVolumeUp,
            0x35 => .NumpadDiv,
            0x38 => .RightAlt,
            0x47 => .Home,
            0x48 => .ArrowUp,
            0x49 => .PageUp,
            0x4B => .ArrowLeft,
            0x4D => .ArrowRight,
            0x4F => .End,
            0x50 => .ArrowDown,
            0x51 => .PageDown,
            0x52 => .Insert,
            0x53 => .Delete,
            0x5B => .LeftSuper,
            0x5C => .RightSuper,
            0x5D => .MenuKey,
            else => return null,
        };
    } else {
        return switch (scan_code) {
            0x01 => .Escape,
            0x02 => .Number1,
            0x03 => .Number2,
            0x04 => .Number3,
            0x05 => .Number4,
            0x06 => .Number5,
            0x07 => .Number6,
            0x08 => .Number7,
            0x09 => .Number8,
            0x0A => .Number9,
            0x0B => .Number0,
            0x0C => .RightOf0,
            0x0D => .LeftOfBackspace,
            0x0E => .Backspace,
            0x0F => .Tab,
            0x10 => .Line1n1,
            0x11 => .Line1n2,
            0x12 => .Line1n3,
            0x13 => .Line1n4,
            0x14 => .Line1n5,
            0x15 => .Line1n6,
            0x16 => .Line1n7,
            0x17 => .Line1n8,
            0x18 => .Line1n9,
            0x19 => .Line1n10,
            0x1A => .Line1n11,
            0x1B => .Line1n12,
            0x2B => .Line1n13,
            0x1C => .Enter,
            0x1D => .LeftCtrl,
            0x1E => .Line2n1,
            0x1F => .Line2n2,
            0x20 => .Line2n3,
            0x21 => .Line2n4,
            0x22 => .Line2n5,
            0x23 => .Line2n6,
            0x24 => .Line2n7,
            0x25 => .Line2n8,
            0x26 => .Line2n9,
            0x27 => .Line2n10,
            0x28 => .Line2n11,
            0x29 => .LeftOf1,
            0x2A => .LeftShift,
            0x2C => .Line3n1,
            0x2D => .Line3n2,
            0x2E => .Line3n3,
            0x2F => .Line3n4,
            0x30 => .Line3n5,
            0x31 => .Line3n6,
            0x32 => .Line3n7,
            0x33 => .Line3n8,
            0x34 => .Line3n9,
            0x35 => .Line3n10,
            0x36 => .RightShift,
            0x37 => .NumpadMul,
            0x38 => .LeftAlt,
            0x39 => .Spacebar,
            0x3A => .CapsLock,
            0x3B => .F1,
            0x3C => .F2,
            0x3D => .F3,
            0x3E => .F4,
            0x3F => .F5,
            0x40 => .F6,
            0x41 => .F7,
            0x42 => .F8,
            0x43 => .F9,
            0x44 => .F10,
            0x45 => .Numlock,
            0x46 => .ScrollLock,
            0x47 => .Numpad7,
            0x48 => .Numpad8,
            0x49 => .Numpad9,
            0x4A => .NumpadSub,
            0x4B => .Numpad4,
            0x4C => .Numpad5,
            0x4D => .Numpad6,
            0x4E => .NumpadAdd,
            0x4F => .Numpad1,
            0x50 => .Numpad2,
            0x51 => .Numpad3,
            0x52 => .Numpad0,
            0x53 => .NumpadPoint,
            // ...
            0x56 => .NonUsBackslash,
            0x57 => .F11,
            0x58 => .F12,
            else => return null,
        };
    }
}

fn standardKey(extended: bool, scan_code: u8) void {
    defer keyboard_buffer.resize(0) catch unreachable;
    const location = keyLocation(extended, scan_code & 0x7F) orelse {
        const kind: []const u8 = if (extended) "extended scancode" else "scancode";
        logger.warn("Unknown {s}: 0x{X}", .{ kind, scan_code & 0x7F });
        return;
    };
    _ = input.enqueueKeyboardEvent(.{ .location = location, .pressed = scan_code & 0x80 == 0 });
}

fn generateEvent() void {
    const buffer = keyboard_buffer.slice();
    switch (buffer[0]) {
        0xE1 => if (finishSequence(1, "\x1D\x45\xE1\x9D\xC5")) {
            _ = input.enqueueKeyboardEvent(.{ .location = .PauseBreak, .pressed = true });
            _ = input.enqueueKeyboardEvent(.{ .location = .PauseBreak, .pressed = false });
        },
        0xE0 => if (keyboard_buffer.len >= 2) {
            switch (buffer[1]) {
                0x2A => if (finishSequence(2, &.{ 0xE0, 0x37 })) {
                    _ = input.enqueueKeyboardEvent(.{ .location = .PrintScreen, .pressed = true });
                },
                0xB7 => if (finishSequence(2, &.{ 0xE0, 0xAA })) {
                    _ = input.enqueueKeyboardEvent(.{ .location = .PrintScreen, .pressed = false });
                },
                else => |byte| standardKey(true, byte),
            }
        },
        else => |byte| standardKey(false, byte),
    }
}

const Ps2KeyboardContext = struct {
    path: [*:0]const u8,
    has_data_port: bool = false,
    data_port: u16 = 0x60,
    command_port: u16 = 0x64,
    irq: u8 = 0x1,
};

fn keyboardHandler(context: u64) void {
    const ps2_ctx: *Ps2KeyboardContext = @ptrFromInt(context);
    keyboard_buffer.append(arch.in(u8, ps2_ctx.data_port)) catch {};
    generateEvent();
    apic.eoi();
}

fn resourceIterationCallback(
    context: ?*anyopaque,
    resource: ?*acpi.C.uacpi_resource,
) callconv(.C) acpi.C.uacpi_resource_iteration_decision {
    const ps2_ctx: *Ps2KeyboardContext = @ptrCast(@alignCast(context.?));
    switch (resource.?.type) {
        acpi.C.UACPI_RESOURCE_TYPE_IRQ => {
            const irq_resource = &resource.?.unnamed_0.irq;
            std.debug.assert(irq_resource.num_irqs == 1);
            ps2_ctx.irq = irq_resource.irqs()[0];
        },
        acpi.C.UACPI_RESOURCE_TYPE_IO => if (!ps2_ctx.has_data_port) {
            // Assume first IO resource is the data port
            ps2_ctx.data_port = resource.?.unnamed_0.io.minimum;
            ps2_ctx.has_data_port = true;
        } else {
            ps2_ctx.command_port = resource.?.unnamed_0.io.minimum;
        },
        else => {},
    }
    return acpi.C.UACPI_RESOURCE_ITERATION_CONTINUE;
}

pub fn init(acpi_node: ?*acpi.C.uacpi_namespace_node) !void {
    const path = acpi.C.uacpi_namespace_node_generate_absolute_path(acpi_node);
    logger.info("Found PS/2 keyboard controller at {s}", .{path});

    const context = try root.allocator.create(Ps2KeyboardContext);
    context.* = .{ .path = path };

    var status: acpi.C.uacpi_status = undefined;
    var resources: ?*acpi.C.uacpi_resources = null;
    defer acpi.C.uacpi_free_resources(resources);

    status = acpi.C.uacpi_get_current_resources(acpi_node, &resources);
    if (status != acpi.C.UACPI_STATUS_OK) {
        logger.err(
            "Failed to get resources for PS/2 keyboard controller {s}: {s}",
            .{ path, acpi.C.uacpi_status_to_string(status) },
        );
        return;
    }

    status = acpi.C.uacpi_for_each_resource(resources, resourceIterationCallback, context);
    if (status != acpi.C.UACPI_STATUS_OK) {
        logger.err(
            "Failed to iterate resources for PS/2 keyboard controller {s}: {s}",
            .{ path, acpi.C.uacpi_status_to_string(status) },
        );
        return;
    }

    logger.debug("PS/2 keyboard controller IRQ: {d}", .{context.irq});
    logger.debug(
        "PS/2 keyboard controller data port: 0x{X}, command port: 0x{X}",
        .{ context.data_port, context.command_port },
    );

    const keyboard_vector = interrupts.allocateVector();
    interrupts.registerHandlerWithContext(keyboard_vector, keyboardHandler, @intFromPtr(context));
    apic.routeIrq(context.irq, 0, keyboard_vector);

    // Flush the keyboard buffer
    while (arch.in(u8, context.command_port) & (1 << 0) != 0) {
        _ = arch.in(u8, context.data_port);
    }

    logger.info("PS/2 keyboard controller initialized", .{});
}
