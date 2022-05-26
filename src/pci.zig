const logger = std.log.scoped(.pci);

const root = @import("root");
const std = @import("std");

const arch = @import("arch.zig");
const drivers = @import("drivers.zig");

fn ConfigSpaceField(comptime T: type, comptime offset: usize) type {
    return struct {
        device: Device,

        pub fn read(self: @This()) T {
            return self.device.read(T, offset);
        }

        pub fn write(self: @This(), value: T) void {
            self.device.write(T, offset, value);
        }
    };
}

fn configField(comptime T: type, comptime offset: usize) fn (Device) ConfigSpaceField(T, offset) {
    return struct {
        fn func(self: Device) ConfigSpaceField(T, offset) {
            return .{ .device = self };
        }
    }.func;
}

pub const BarSpace = enum {
    Port,
    Mmio,
};

pub const BarInfo = struct {
    base: u64,
    length: u64,
    kind: BarSpace,
};

pub const Device = struct {
    bus: u8,
    slot: u8,
    function: u8,

    pub const vendor_id = configField(u16, 0x0);
    pub const device_id = configField(u16, 0x2);
    pub const command = configField(u16, 0x4);
    pub const prog_if = configField(u8, 0x9);
    pub const subclass_id = configField(u8, 0xA);
    pub const class_id = configField(u8, 0xB);
    pub const header_type = configField(u8, 0xE);

    fn init(bus: u8, slot: u8, function: u8) Device {
        std.debug.assert(bus & 0b10000000 == 0);
        std.debug.assert(slot & 0b11100000 == 0);
        std.debug.assert(function & 0b11111000 == 0);

        return .{ .bus = bus, .slot = slot, .function = function };
    }

    fn selectField(self: Device, offset: u8) void {
        const slot = @intCast(u8, self.slot << 3) | self.function;
        const address = @intCast(u32, 1 << 31) | @intCast(u32, self.bus) << 16 | @intCast(u32, slot) << 8;

        arch.out(u32, 0xCF8, address | offset);
    }

    pub fn read(self: Device, comptime T: type, offset: u8) T {
        self.selectField(offset);
        return arch.in(T, 0xCFC + @as(u16, offset & 0x3));
    }

    pub fn write(self: Device, comptime T: type, offset: u8, value: T) void {
        self.selectField(offset);
        arch.out(T, 0xCFC + @as(u16, offset & 0x3), value);
    }

    pub fn getBar(self: Device, index: u8) ?BarInfo {
        const pci_out = self.read(u32, 0x10 + index * 4);
        const is_mmio = pci_out & 0b1 == 0; // bit 0, always 1 for IO space BARs
        const is_64bit = (pci_out & 0b110) >> 1 == 2; // bits 1:2, bar type (0 = 32bit, 1 = 64bit)

        if (is_mmio) {
            self.write(u32, 0x10 + index * 4, 0xFFFFFFFF);

            const base = @intCast(u64, pci_out & 0xFFFFFFF0);
            const ones_out = self.read(u32, 0x10 + index * 4) & 0xFFFFFFF0;
            const size = ~ones_out +% 1;

            if (base == 0) {
                return null;
            }

            self.write(u32, 0x10 + index * 4, pci_out);

            if (is_64bit) {
                const pci_out_high = self.read(u32, 0x14 + index * 4);
                const base_high = @intCast(u64, pci_out_high) << 32;

                return BarInfo{
                    .base = base | base_high << 32,
                    .length = size,
                    .kind = .Mmio,
                };
            }

            return BarInfo{
                .base = base,
                .length = size,
                .kind = .Mmio,
            };
        } else {
            const base = @intCast(u64, pci_out & 0xFFFFFFFC);
            const ones_out = self.read(u32, 0x10 + index * 4) & 0xFFFFFFFC;
            const size = ~ones_out +% 1;

            if (base == 0) {
                return null;
            }

            return BarInfo{
                .base = base,
                .length = size,
                .kind = .Port,
            };
        }
    }
};

fn checkFunction(device: Device) !void {
    try devices.append(root.allocator, device);

    const vendor_id = device.vendor_id().read();
    const device_id = device.device_id().read();
    const class_id = device.class_id().read();
    const subclass_id = device.subclass_id().read();
    const prog_if = device.prog_if().read();

    logger.info(
        "Found device {X:0>4}:{X:0>4} on bus {}, slot {}, function {}",
        .{ vendor_id, device_id, device.bus, device.slot, device.function },
    );

    inline for (@typeInfo(@TypeOf(drivers.pci_drivers)).Struct.fields) |field| {
        const driver = @field(drivers.pci_drivers, field.name);
        const discovery = @as(drivers.PciDriverDiscovery, driver.discovery);

        switch (discovery) {
            .all => try driver.handler(device),
            .id => |id| {
                if (vendor_id == id.vendor and device_id == id.device) {
                    try driver.handler(device);
                }
            },
            .class => |class| {
                if (class_id == class.class_id and subclass_id == class.subclass_id) {
                    if (class.prog_if == null or prog_if == class.prog_if.?) {
                        try driver.handler(device);
                    }
                }
            },
        }
    }
}

fn checkSlot(bus: u8, slot: u8) !void {
    const device = Device.init(bus, slot, 0);

    if (device.vendor_id().read() == 0xFFFF) {
        return;
    }

    try checkFunction(device);

    // Check if the device is a multi-function device
    if (device.header_type().read() & 0x80 != 0) {
        var function: u8 = 1;

        while (function < 8) : (function += 1) {
            const func_header = Device.init(bus, slot, function);

            if (func_header.vendor_id().read() != 0xFFFF) {
                try checkFunction(func_header);
            }
        }
    }
}

fn checkBus(bus: u8) !void {
    var device: u8 = 0;

    while (device < 32) : (device += 1) {
        try checkSlot(bus, device);
    }
}

var devices: std.ArrayListUnmanaged(Device) = .{};

pub fn init() !void {
    const root_bus = Device.init(0, 0, 0);

    if (root_bus.header_type().read() & 0x80 == 0) {
        try checkBus(0);
    } else {
        var function: u8 = 0;

        while (function < 8) : (function += 1) {
            const bus_header = Device.init(0, 0, function);

            if (bus_header.vendor_id().read() != 0xFFFF) {
                try checkBus(function);
            }
        }
    }
}
