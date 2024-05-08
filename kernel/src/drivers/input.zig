const logger = std.log.scoped(.input);

const std = @import("std");

// zig fmt: off
pub const KeyLocation = enum {
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
    NonUsPound, Enter,

    // Third QWERTY row
    LeftShift, NonUsBackslash, Line3n1, Line3n2, Line3n3,
    Line3n4, Line3n5, Line3n6, Line3n7, Line3n8, Line3n9,
    Line3n10, RightShift,

    // Bottom row
    LeftCtrl, LeftSuper, LeftAlt, Spacebar, RightAlt,
    RightSuper, MenuKey, RightCtrl,

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
};

pub const KeyboardState = struct {
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

const RingWaitQueue = @import("../containers/ring_buffer.zig").RingWaitQueue;

var keyboard_queue: RingWaitQueue(KeyboardEvent, 128) = .{};

pub var keyboard_state: KeyboardState = .{};

pub fn enqueueKeyboardEvent(event: KeyboardEvent) bool {
    return keyboard_queue.push(event);
}

pub fn dequeueKeyboardEvent() KeyboardEvent {
    const event = keyboard_queue.get();
    const dropped = keyboard_queue.dropped();

    if (dropped > 0) {
        logger.warn("Dropped {} keyboard events", .{dropped});
    }

    keyboard_state.processEvent(event);
    return event;
}
