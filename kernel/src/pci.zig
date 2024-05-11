const logger = std.log.scoped(.pci);

const root = @import("root");
const std = @import("std");

const arch = @import("arch.zig");
const drivers = @import("drivers.zig");
const virt = @import("virt.zig");

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

pub const Capability = struct {
    device: Device,
    offset: u8,

    pub fn next(self: *Capability) ?Capability {
        if (self.offset == 0) {
            return null;
        } else {
            const result = self.*;
            self.offset = self.device.read(u8, self.offset + 0x01);
            return result;
        }
    }

    pub fn vendor(self: Capability) u8 {
        return self.device.read(u8, self.offset + 0x00);
    }

    pub fn read(self: Capability, comptime T: type, offset: u8) T {
        return self.device.read(T, self.offset + offset);
    }

    pub fn write(self: Capability, comptime T: type, offset: u8, value: T) void {
        self.device.write(T, self.offset + offset, value);
    }
};

pub const Msi = struct {
    pci_cap: Capability,
    is_msix: bool,

    pub fn enable(self: Msi, lapic_id: u32, vector: u8) void {
        const msi_addr: u64 = 0xFEE00000 | (lapic_id << 12);
        const msi_data = @as(u16, vector); // | 1 << 15); // Level trigger

        if (self.is_msix) {
            logger.info("Enabling MSI-X for device, lapic_id: {}, vector: {}", .{ lapic_id, vector });
            const msi_table_info = self.pci_cap.read(u32, 4);
            const msi_table_bar = self.pci_cap.device.getBar(@truncate(msi_table_info & 0x7));
            const msi_table = virt.asHigherHalfUncached([*]volatile u32, msi_table_bar.?.base + (msi_table_info & 0xFFFFFFF8));
            msi_table[3] = 0;
            msi_table[2] = msi_data;
            msi_table[0] = @truncate(msi_addr);
            msi_table[1] = @truncate(msi_addr >> 32);

            // Set the enable bit in the message control register
            self.pci_cap.write(u16, 2, self.pci_cap.read(u16, 2) | (1 << 15));
        } else {
            logger.info("Enabling MSI for device , lapic_id: {}, vector: {}", .{ lapic_id, vector });

            self.pci_cap.write(u32, 0x4, @truncate(msi_addr));
            const message_control = self.pci_cap.read(u16, 2);

            // 64-bit
            if (message_control & (1 << 7) != 0) {
                self.pci_cap.write(u32, 0x8, @as(u32, @intCast(msi_addr >> 32)));
                self.pci_cap.write(u16, 0xC, msi_data);
            } else {
                self.pci_cap.write(u16, 0x8, msi_data);
            }

            // Set the enable bit in the message control register
            self.pci_cap.write(u16, 2, message_control | (1 << 0));
        }

        // Clear the interrupt disable bit in the command register
        const command_register = self.pci_cap.device.command();
        command_register.write(command_register.read() & ~@as(u16, 1 << 10));
    }
};

pub const Device = struct {
    bus: u8,
    slot: u8,
    function: u8,

    pub const vendor_id = configField(u16, 0x00);
    pub const device_id = configField(u16, 0x02);
    pub const command = configField(u16, 0x04);
    pub const status = configField(u16, 0x06);
    pub const prog_if = configField(u8, 0x09);
    pub const header_type = configField(u8, 0x0E);
    pub const class_id = configField(u8, 0x0B);
    pub const subclass_id = configField(u8, 0x0A);
    pub const secondary_bus = configField(u8, 0x19);
    pub const cap_ptr = configField(u8, 0x34);
    pub const int_line = configField(u8, 0x3C);
    pub const int_pin = configField(u8, 0x3D);

    fn init(bus: u8, slot: u8, function: u8) Device {
        std.debug.assert(bus & 0b10000000 == 0);
        std.debug.assert(slot & 0b11100000 == 0);
        std.debug.assert(function & 0b11111000 == 0);

        return .{ .bus = bus, .slot = slot, .function = function };
    }

    fn selectField(self: Device, offset: u8) void {
        const slot = @as(u8, @intCast(self.slot << 3)) | self.function;
        const address = @as(u32, @intCast(1 << 31)) | @as(u32, @intCast(self.bus)) << 16 | @as(u32, @intCast(slot)) << 8;

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

            const base = @as(u64, @intCast(pci_out & 0xFFFFFFF0));
            const ones_out = self.read(u32, 0x10 + index * 4) & 0xFFFFFFF0;
            const size = ~ones_out +% 1;

            if (base == 0) {
                return null;
            }

            self.write(u32, 0x10 + index * 4, pci_out);

            if (is_64bit) {
                const pci_out_high = self.read(u32, 0x14 + index * 4);
                const base_high = @as(u64, @intCast(pci_out_high)) << 32;

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
            const base = @as(u64, @intCast(pci_out & 0xFFFFFFFC));
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

    pub fn enableDma(self: Device) void {
        self.command().write(self.command().read() | 0x6);
    }

    pub fn capabilities(self: Device) Capability {
        if (self.status().read() & (1 << 4) == 0) {
            return .{ .device = undefined, .offset = 0 };
        } else {
            return .{ .device = self, .offset = self.cap_ptr().read() & 0xFC };
        }
    }

    pub fn getMsi(self: Device) ?Msi {
        var caps = self.capabilities();
        var msi: ?Msi = null;

        while (caps.next()) |cap| {
            switch (cap.vendor()) {
                0x5 => if (msi == null) {
                    msi = Msi{ .pci_cap = cap, .is_msix = false };
                },
                0x11 => return Msi{ .pci_cap = cap, .is_msix = true },
                else => {},
            }
        }

        return msi;
    }
};

fn checkFunction(device: Device) anyerror!void {
    const vendor_id = device.vendor_id().read();
    const device_id = device.device_id().read();
    const class_id = device.class_id().read();
    const subclass_id = device.subclass_id().read();
    const prog_if = device.prog_if().read();

    logger.info(
        "Found device {X:0>4}:{X:0>4} on bus {}, slot {}, function {}",
        .{ vendor_id, device_id, device.bus, device.slot, device.function },
    );

    inline for (drivers.pci_drivers) |driver| {
        switch (@as(drivers.PCIDriverDiscovery, driver.discovery)) {
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

    if (device.header_type().read() & 0x80 == 0) {
        return;
    }

    for (1..8) |func| {
        const it_device = Device.init(bus, slot, @intCast(func));

        if (it_device.vendor_id().read() != 0xFFFF) {
            try checkFunction(it_device);
        }
    }
}

pub fn scanBus(segment: u8, bus: u8) !void {
    _ = segment;

    for (0..32) |i| {
        try checkSlot(bus, @intCast(i));
    }
}
