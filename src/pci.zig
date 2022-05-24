const logger = std.log.scoped(.pci);

const root = @import("root");
const std = @import("std");

const arch = @import("arch.zig");

const PciHeader = struct {
    bus: u8,
    device: u8,
    function: u8,

    fn init(bus: u8, device: u8, function: u8) PciHeader {
        std.debug.assert(bus & 0b10000000 == 0);
        std.debug.assert(device & 0b11100000 == 0);
        std.debug.assert(function & 0b11111000 == 0);

        return .{ .bus = bus, .device = device, .function = function };
    }

    fn readDword(self: *const PciHeader, offset: u8) u32 {
        std.debug.assert(offset & 0x3 == 0); // Make sure offset is 4 byte aligned

        const slot = @intCast(u8, self.device << 3) | self.function;
        const address = @intCast(u32, 1 << 31) | @intCast(u32, self.bus) << 16 | @intCast(u32, slot) << 8;

        arch.out(u32, 0xCF8, address | offset);

        return arch.in(u32, 0xCFC);
    }

    fn readWord(self: *const PciHeader, offset: u8) u16 {
        std.debug.assert(offset & 0x1 == 0); // Make sure offset is 2 byte aligned

        const dword = self.readDword(offset & ~@as(u8, 0x3));
        const word_shift = 16 * ((offset & 0x3) >> 1);

        return @truncate(u16, dword >> @intCast(u5, word_shift));
    }

    fn readByte(self: *const PciHeader, offset: u8) u8 {
        const word = self.readWord(offset & ~@as(u8, 0x1));
        const byte_shift = 8 * (offset & 0x1);

        return @truncate(u8, word >> @intCast(u4, byte_shift));
    }

    fn readVendorId(self: *const PciHeader) u16 {
        return self.readWord(0x0);
    }

    fn readDeviceId(self: *const PciHeader) u16 {
        return self.readWord(0x2);
    }

    fn readHeaderType(self: *const PciHeader) u8 {
        return self.readByte(0xE);
    }
};

fn checkFunction(header: PciHeader) !void {
    try devices.append(root.allocator, header);

    logger.info(
        "Found device {X:0>4}:{X:0>4} on bus {}, slot {}, function {}",
        .{ header.readVendorId(), header.readDeviceId(), header.bus, header.device, header.function },
    );
}

fn checkDevice(bus: u8, device: u8) !void {
    const header = PciHeader.init(bus, device, 0);

    if (header.readVendorId() == 0xFFFF) {
        return;
    }

    try checkFunction(header);

    // Check if the device is a multi-function device
    if (header.readHeaderType() & 0x80 != 0) {
        var function: u8 = 1;

        while (function < 8) : (function += 1) {
            const func_header = PciHeader.init(bus, device, function);

            if (func_header.readVendorId() != 0xFFFF) {
                try checkFunction(func_header);
            }
        }
    }
}

fn checkBus(bus: u8) !void {
    var device: u8 = 0;

    while (device < 32) : (device += 1) {
        try checkDevice(bus, device);
    }
}

var devices: std.ArrayListUnmanaged(PciHeader) = .{};

pub fn init() !void {
    const root_bus = PciHeader.init(0, 0, 0);

    if (root_bus.readHeaderType() & 0x80 == 0) {
        try checkBus(0);
    } else {
        var function: u8 = 0;

        while (function < 8) : (function += 1) {
            const bus_header = PciHeader.init(0, 0, function);

            if (bus_header.readVendorId() != 0xFFFF) {
                try checkBus(function);
            }
        }
    }
}
