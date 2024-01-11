const logger = std.log.scoped(.ps2);

const std = @import("std");

const arch = @import("../arch.zig");
const apic = @import("../apic.zig");
const interrupts = @import("../interrupts.zig");
const scheduler = @import("../scheduler.zig");

const RingWaitQueue = @import("../containers/ring_buffer.zig").RingWaitQueue;

// zig fmt: off
const KeyLocation = enum {
    Escape, F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12,
    F13, F14, F15, F16, F17, F18, F19, F20, F21, F22, F23, F24,

    // Number row
    LeftOf1, Number1, Number2, Number3, Number4, Number5,
    Number6, Number7, Number8, Number9, Number0, RightOf0,
    LeftOfBackspace, Backspace,

    // First QWERTY row
    Tab, Line1n1, Line1n2, Line1n3, Line1n4, Line1n5,
    Line1n6, Line1n7, Line1n8, Line1n9, Line1n10, Line1n11,
    Line1n12, Line1n13,

    // Second QWERTY row
    CapsLock, Line2n1, Line2n2, Line2n3, Line2n4, Line2n5,
    Line2n6, Line2n7, Line2n8, Line2n9, Line2n10, Line2n11,
    Line2n12, Enter,

    // Third QWERTY row
    LeftShift, RightOfLshift, Line3n1, Line3n2, Line3n3,
    Line3n4, Line3n5, Line3n6, Line3n7, Line3n8, Line3n9,
    Line3n10, RightShift,

    // Bottom row
    LeftCtrl, RightCtrl, LeftSuper, RightSuper, LeftAlt,
    RightAlt, Spacebar, OptionKey,

    // Directional keys
    ArrowUp, ArrowLeft, ArrowDown, ArrowRight,

    // Group above directional keys
    PrintScreen, PauseBreak, ScrollLock, Insert, Home, PageUp,
    Delete, End, PageDown,

    // Numpad
    Numlock, NumpadDiv, NumpadMul, Numpad7, Numpad8, Numpad9,
    NumpadSub, Numpad4, Numpad5, Numpad6, NumpadAdd, Numpad1,
    Numpad2, Numpad3, Numpad0, NumpadPoint, NumpadEnter,

    // Multimedia keys
    MediaStop, MediaRewind, MediaPausePlay, MediaForward,
    MediaMute, MediaVolumeUp, MediaVolumeDown,
};
// zig fmt: on

pub const KeyboardEvent = struct {
    location: KeyLocation,
    pressed: bool,

    fn init(location: KeyLocation, pressed: bool) KeyboardEvent {
        return KeyboardEvent{ .location = location, .pressed = pressed };
    }
};

const KeyboardState = struct {
    const State = std.PackedIntArray(bool, @typeInfo(KeyLocation).Enum.fields.len);

    is_pressed: State = blk: {
        @setEvalBranchQuota(9999999);

        break :blk State.initAllTo(false);
    },

    pub fn isPressed(self: *KeyboardState, location: KeyLocation) bool {
        return self.is_pressed.get(@intFromEnum(location));
    }

    pub fn isCtrlPressed(self: *KeyboardState) bool {
        return self.isPressed(.LeftCtrl) or self.isPressed(.RightCtrl);
    }

    pub fn isAltPressed(self: *KeyboardState) bool {
        return self.isPressed(.LeftAlt) or self.isPressed(.RightAlt);
    }

    pub fn isShiftPressed(self: *KeyboardState) bool {
        return self.isPressed(.LeftShift) or self.isPressed(.RightShift);
    }

    pub fn isSuperPressed(self: *KeyboardState) bool {
        return self.isPressed(.LeftSuper) or self.isPressed(.RightSuper);
    }

    fn processEvent(self: *KeyboardState, event: KeyboardEvent) void {
        self.is_pressed.set(@intFromEnum(event.location), event.pressed);
    }
};

const Buffer = std.BoundedArray(u8, 8);

var keyboard_queue: RingWaitQueue(KeyboardEvent, 128) = .{};
var keyboard_buffer: Buffer = Buffer.init(0) catch unreachable;

pub var keyboard_state: KeyboardState = .{};

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

fn keyLocation(extended: bool, scan_code: u8) ?KeyLocation {
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
            0x5D => .OptionKey,
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
            0x2B => .Line2n12,
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
            0x56 => .RightOfLshift,
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

    _ = keyboard_queue.push(KeyboardEvent.init(location, scan_code & 0x80 == 0));
}

fn generateEvent() void {
    const buffer = keyboard_buffer.slice();

    switch (buffer[0]) {
        0xE1 => if (finishSequence(1, "\x1D\x45\xE1\x9D\xC5")) {
            _ = keyboard_queue.push(KeyboardEvent.init(.PauseBreak, true));
            _ = keyboard_queue.push(KeyboardEvent.init(.PauseBreak, false));
        },
        0xE0 => if (keyboard_buffer.len >= 2) {
            switch (buffer[1]) {
                0x2A => if (finishSequence(2, &.{ 0xE0, 0x37 })) {
                    _ = keyboard_queue.push(KeyboardEvent.init(.PrintScreen, true));
                },
                0xB7 => if (finishSequence(2, &.{ 0xE0, 0xAA })) {
                    _ = keyboard_queue.push(KeyboardEvent.init(.PrintScreen, false));
                },
                else => |byte| standardKey(true, byte),
            }
        },
        else => |byte| standardKey(false, byte),
    }
}

fn keyboardHandler(frame: *interrupts.InterruptFrame) void {
    _ = frame;

    keyboard_buffer.append(arch.in(u8, 0x60)) catch {};

    generateEvent();

    apic.eoi();
}

pub fn getKeyboardEvent() KeyboardEvent {
    const event = keyboard_queue.get();
    const dropped = keyboard_queue.dropped();

    if (dropped > 0) {
        logger.debug("Dropped {} keystrokes", .{dropped});
    }

    keyboard_state.processEvent(event);

    return event;
}

pub fn init() void {
    const keyboard_vector = interrupts.allocateVector();

    interrupts.registerHandler(keyboard_vector, keyboardHandler);

    apic.routeIrq(1, apic.localApicId(), keyboard_vector);

    _ = arch.in(u8, 0x60);
}
