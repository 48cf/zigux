const logger = std.log.scoped(.usb);

const std = @import("std");

const xhci = @import("./xhci.zig");
const input = @import("./input.zig");
const phys = @import("../phys.zig");

fn keyboardUsageToKeyLocation(scancode: usize) ?input.KeyLocation {
    return switch (scancode) {
        0x4 => .Line2n1, // Keyboard A
        0x5 => .Line3n5, // Keyboard B
        0x6 => .Line3n3, // Keyboard C
        0x7 => .Line2n3, // Keyboard D
        0x8 => .Line1n3, // Keyboard E
        0x9 => .Line2n4, // Keyboard F
        0xA => .Line2n5, // Keyboard G
        0xB => .Line2n6, // Keyboard H
        0xC => .Line1n8, // Keyboard I
        0xD => .Line2n7, // Keyboard J
        0xE => .Line2n8, // Keyboard K
        0xF => .Line2n9, // Keyboard L
        0x10 => .Line3n7, // Keyboard M
        0x11 => .Line3n6, // Keyboard N
        0x12 => .Line1n9, // Keyboard O
        0x13 => .Line1n10, // Keyboard P
        0x14 => .Line1n1, // Keyboard Q
        0x15 => .Line1n4, // Keyboard R
        0x16 => .Line2n2, // Keyboard S
        0x17 => .Line1n5, // Keyboard T
        0x18 => .Line1n7, // Keyboard U
        0x19 => .Line3n4, // Keyboard V
        0x1A => .Line1n2, // Keyboard W
        0x1B => .Line3n2, // Keyboard X
        0x1C => .Line1n6, // Keyboard Y
        0x1D => .Line3n1, // Keyboard Z
        0x1E => .Number1, // Keyboard 1
        0x1F => .Number2, // Keyboard 2
        0x20 => .Number3, // Keyboard 3
        0x21 => .Number4, // Keyboard 4
        0x22 => .Number5, // Keyboard 5
        0x23 => .Number6, // Keyboard 6
        0x24 => .Number7, // Keyboard 7
        0x25 => .Number8, // Keyboard 8
        0x26 => .Number9, // Keyboard 9
        0x27 => .Number0, // Keyboard 0
        0x28 => .Enter, // Keyboard Return (Enter)
        0x29 => .Escape, // Keyboard Escape
        0x2A => .Backspace, // Keyboard Delete (Backspace)
        0x2B => .Tab, // Keyboard Tab
        0x2C => .Spacebar, // Keyboard Spacebar
        0x2D => .RightOf0, // Keyboard - and (underscore)
        0x2E => .LeftOfBackspace, // Keyboard = and +
        0x2F => .Line1n11, // Keyboard [ and {
        0x30 => .Line1n12, // Keyboard ] and }
        0x31 => .Line1n13, // Keyboard \ and |
        0x32 => .NonUsPound, // Keyboard Non-US # and ~
        0x33 => .Line2n10, // Keyboard ; and :
        0x34 => .Line2n11, // Keyboard ' and "
        0x35 => .LeftOf1, // Keyboard ` and ~
        0x36 => .Line3n8, // Keyboard , and <
        0x37 => .Line3n9, // Keyboard . and >
        0x38 => .Line3n10, // Keyboard / and ?
        0x39 => .CapsLock, // Keyboard Caps Lock
        0x3A => .F1, // Keyboard F1
        0x3B => .F2, // Keyboard F2
        0x3C => .F3, // Keyboard F3
        0x3D => .F4, // Keyboard F4
        0x3E => .F5, // Keyboard F5
        0x3F => .F6, // Keyboard F6
        0x40 => .F7, // Keyboard F7
        0x41 => .F8, // Keyboard F8
        0x42 => .F9, // Keyboard F9
        0x43 => .F10, // Keyboard F10
        0x44 => .F11, // Keyboard F11
        0x45 => .F12, // Keyboard F12
        0x46 => .PrintScreen, // Keyboard PrintScreen
        0x47 => .ScrollLock, // Keyboard Scroll Lock
        0x48 => .PauseBreak, // Keyboard Pause
        0x49 => .Insert, // Keyboard Insert
        0x4A => .Home, // Keyboard Home
        0x4B => .PageUp, // Keyboard PageUp
        0x4C => .Delete, // Keyboard Delete Forward
        0x4D => .End, // Keyboard End
        0x4E => .PageDown, // Keyboard PageDown
        0x4F => .ArrowRight, // Keyboard RightArrow
        0x50 => .ArrowLeft, // Keyboard LeftArrow
        0x51 => .ArrowDown, // Keyboard DownArrow
        0x52 => .ArrowUp, // Keyboard UpArrow
        0x53 => .Numlock, // Keyboard Num Lock and Clear
        0x54 => .NumpadDiv, // Keypad /
        0x55 => .NumpadMul, // Keypad *
        0x56 => .NumpadSub, // Keypad -
        0x57 => .NumpadAdd, // Keypad +
        0x58 => .NumpadEnter, // Keypad Enter
        0x59 => .Numpad1, // Keypad 1 and End
        0x5A => .Numpad2, // Keypad 2 and Down Arrow
        0x5B => .Numpad3, // Keypad 3 and PageDn
        0x5C => .Numpad4, // Keypad 4 and Left Arrow
        0x5D => .Numpad5, // Keypad 5
        0x5E => .Numpad6, // Keypad 6 and Right Arrow
        0x5F => .Numpad7, // Keypad 7 and Home
        0x60 => .Numpad8, // Keypad 8 and Up Arrow
        0x61 => .Numpad9, // Keypad 9 and PageUp
        0x62 => .Numpad0, // Keypad 0 and Insert
        0x63 => .NumpadPoint, // Keypad . and Delete
        0x64 => .NonUsBackslash, // Keyboard Non-US \ and |
        // 0x65 Keyboard Application
        // 0x66 Keyboard Power
        // 0x67 Keypad =
        0x68 => .F13, // Keyboard F13
        0x69 => .F14, // Keyboard F14
        0x6A => .F15, // Keyboard F15
        0x6B => .F16, // Keyboard F16
        0x6C => .F17, // Keyboard F17
        0x6D => .F18, // Keyboard F18
        0x6E => .F19, // Keyboard F19
        0x6F => .F20, // Keyboard F20
        0x70 => .F21, // Keyboard F21
        0x71 => .F22, // Keyboard F22
        0x72 => .F23, // Keyboard F23
        0x73 => .F24, // Keyboard F24
        // ...
        0xE0 => .LeftCtrl, // Keyboard LeftControl
        0xE1 => .LeftShift, // Keyboard LeftShift
        0xE2 => .LeftAlt, // Keyboard LeftAlt
        0xE3 => .LeftSuper, // Keyboard Left GUI
        0xE4 => .RightCtrl, // Keyboard RightControl
        0xE5 => .RightShift, // Keyboard RightShift
        0xE6 => .RightAlt, // Keyboard RightAlt
        0xE7 => .RightSuper, // Keyboard Right GUI
        else => null,
    };
}

const HidKeyboardDevice = struct {
    const KeyState = std.PackedIntArray(bool, 256);

    poll_ep: Endpoint = undefined,
    report_buffer: u64 = 0,
    key_state: KeyState = blk: {
        @setEvalBranchQuota(9999999);
        break :blk KeyState.initAllTo(false);
    },

    fn configure(self: *@This(), device: *Device, controller: *xhci.Controller, slot_id: usize) void {
        controller.sendControlTransfer(@intCast(slot_id), 0, 0x21, 0xB, 0, device.interface_id, 0, 0, false);
        controller.sendControlTransfer(@intCast(slot_id), 0, 0x21, 0xA, 0, device.interface_id, 0, 0, false);
        controller.enableEndpoint(slot_id, self.poll_ep);
        self.report_buffer = phys.allocate(1, true).?;
    }

    fn onConfigured(self: *@This(), device: *Device, controller: *xhci.Controller, slot_id: usize) void {
        _ = device;
        controller.sendDataTransfer(@intCast(slot_id), self.poll_ep.address, 8, self.report_buffer, true);
    }

    fn onDataTransferComplete(
        self: *@This(),
        device: *Device,
        controller: *xhci.Controller,
        endpoint_addr: u8,
        slot_id: usize,
        data: []const u8,
    ) bool {
        _ = device;
        if (endpoint_addr == self.poll_ep.address) {
            defer controller.sendDataTransfer(@intCast(slot_id), self.poll_ep.address, 8, self.report_buffer, true);
            logger.debug("Keyboard report data: {any}", .{std.fmt.fmtSliceHexUpper(data)});

            const modifiers = data[0];
            const scancodes = data[2..];

            if (scancodes[0] == 0x1) {
                return true;
            }

            for (0..8, 0xE0..) |i, scancode| {
                const state = modifiers & (@as(u8, 1) << @intCast(i)) != 0;
                const previous_state = self.key_state.get(scancode);
                if (state != previous_state) {
                    self.key_state.set(scancode, state);
                    _ = input.enqueueKeyboardEvent(.{
                        .location = keyboardUsageToKeyLocation(scancode).?,
                        .pressed = state,
                    });
                }
            }

            for (0x0..0xE0) |scancode| {
                if (!self.key_state.get(scancode)) {
                    continue;
                }

                if (std.mem.indexOfScalar(u8, scancodes, @intCast(scancode)) == null) {
                    self.key_state.set(scancode, false);
                    _ = input.enqueueKeyboardEvent(.{
                        .location = keyboardUsageToKeyLocation(scancode) orelse continue,
                        .pressed = false,
                    });
                }
            }

            for (scancodes) |scancode| {
                if (scancode != 0 and !self.key_state.get(scancode)) {
                    self.key_state.set(scancode, true);
                    _ = input.enqueueKeyboardEvent(.{
                        .location = keyboardUsageToKeyLocation(scancode) orelse {
                            logger.warn("Unhandled scancode: {X}", .{scancode});
                            continue;
                        },
                        .pressed = true,
                    });
                }
            }

            return true;
        }

        return false;
    }
};

const HidMouseDevice = struct {
    poll_ep: Endpoint = undefined,
    report_buffer: u64 = 0,

    fn configure(self: *@This(), device: *Device, controller: *xhci.Controller, slot_id: usize) void {
        controller.sendControlTransfer(@intCast(slot_id), 0, 0x21, 0xB, 0, device.interface_id, 0, 0, false);
        controller.sendControlTransfer(@intCast(slot_id), 0, 0x21, 0xA, 0, device.interface_id, 0, 0, false);
        controller.enableEndpoint(slot_id, self.poll_ep);
        self.report_buffer = phys.allocate(1, true).?;
    }

    fn onConfigured(self: *@This(), device: *Device, controller: *xhci.Controller, slot_id: usize) void {
        _ = device;
        controller.sendDataTransfer(@intCast(slot_id), self.poll_ep.address, 32, self.report_buffer, true);
    }

    fn onDataTransferComplete(
        self: *@This(),
        device: *Device,
        controller: *xhci.Controller,
        endpoint_addr: u8,
        slot_id: usize,
        data: []const u8,
    ) bool {
        _ = device;
        if (endpoint_addr == self.poll_ep.address) {
            defer controller.sendDataTransfer(@intCast(slot_id), self.poll_ep.address, 8, self.report_buffer, true);
            logger.debug("Mouse report data: {any}", .{std.fmt.fmtSliceHexUpper(data)});
            return true;
        }

        return false;
    }
};

pub const Endpoint = extern struct {
    length: u8,
    descriptor_type: u8,
    address: u8,
    attributes: u8,
    packet_size: u16 align(1),
    interval: u8,
};

pub const Device = struct {
    interface_id: u8 = undefined,
    impl: union(enum) {
        hid_keyboard: HidKeyboardDevice,
        hid_mouse: HidMouseDevice,
    },

    pub fn configure(self: *@This(), controller: *xhci.Controller, slot_id: usize) void {
        switch (self.impl) {
            .hid_keyboard => |*hid| hid.configure(self, controller, slot_id),
            .hid_mouse => |*hid| hid.configure(self, controller, slot_id),
        }
    }

    pub fn onConfigured(self: *@This(), controller: *xhci.Controller, slot_id: usize) void {
        switch (self.impl) {
            .hid_keyboard => |*hid| hid.onConfigured(self, controller, slot_id),
            .hid_mouse => |*hid| hid.onConfigured(self, controller, slot_id),
        }
    }

    pub fn onDataTransferComplete(
        self: *@This(),
        controller: *xhci.Controller,
        endpoint_addr: u8,
        slot_id: usize,
        data: []const u8,
    ) bool {
        switch (self.impl) {
            .hid_keyboard => |*hid| return hid.onDataTransferComplete(self, controller, endpoint_addr, slot_id, data),
            .hid_mouse => |*hid| return hid.onDataTransferComplete(self, controller, endpoint_addr, slot_id, data),
        }

        return false;
    }

    fn handleDescriptor(self: *@This(), descriptor: []const u8) void {
        const descriptor_type = descriptor[1];
        switch (descriptor_type) {
            5 => {
                const endpoint = std.mem.bytesToValue(Endpoint, descriptor[0..7]);
                switch (self.impl) {
                    .hid_keyboard => |*hid| {
                        if ((endpoint.attributes & 0b11) == 0b11 and (endpoint.address & 0x80) != 0) {
                            logger.debug("Found HID keyboard polling endpoint {any}", .{endpoint});
                            hid.poll_ep = endpoint;
                        }
                    },
                    .hid_mouse => |*hid| {
                        if ((endpoint.attributes & 0b11) == 0b11 and (endpoint.address & 0x80) != 0) {
                            logger.debug("Found HID mouse polling endpoint {any}", .{endpoint});
                            hid.poll_ep = endpoint;
                        }
                    },
                }
            },
            else => {},
        }
    }
};

pub fn identifyDevice(config_descriptor: []const u8) !std.BoundedArray(Device, 8) {
    var devices = std.BoundedArray(Device, 8){};
    var offset = @as(usize, config_descriptor[0]); // bLength
    var current_device: ?*Device = null;
    while (offset + 2 <= config_descriptor.len) {
        const length = config_descriptor[offset];
        const descriptor_type = config_descriptor[offset + 1];
        switch (descriptor_type) {
            4 => {
                const interface_class = config_descriptor[offset + 5];
                const interface_subclass = config_descriptor[offset + 6];
                const interface_protocol = config_descriptor[offset + 7];

                if (interface_class == 0x3) {
                    switch (interface_subclass) {
                        0x1 => switch (interface_protocol) {
                            0x1 => {
                                current_device = try devices.addOne();
                                current_device.?.* = .{ .impl = .{ .hid_keyboard = .{} } };
                            },
                            0x2 => {
                                current_device = try devices.addOne();
                                current_device.?.* = .{ .impl = .{ .hid_mouse = .{} } };
                            },
                            else => logger.warn("Unhandled HID boot protocol {d}", .{interface_protocol}),
                        },
                        else => logger.warn("Unhandled HID subclass {d}", .{interface_subclass}),
                    }
                } else {
                    logger.warn(
                        "Unhandled interface class {d} (subclass {d}, protocol {d})",
                        .{ interface_class, interface_subclass, interface_protocol },
                    );
                }

                if (current_device) |device| {
                    device.interface_id = config_descriptor[offset + 2];
                }
            },
            else => if (current_device) |device| {
                device.handleDescriptor(config_descriptor[offset..][0..length]);
            },
        }
        offset += length;
    }
    return devices;
}
