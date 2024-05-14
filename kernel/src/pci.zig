const logger = std.log.scoped(.pci);

const root = @import("root");
const std = @import("std");

const acpi = @import("./acpi.zig");
const arch = @import("./arch.zig");
const drivers = @import("./drivers.zig");
const uacpi = @import("./uacpi.zig");
const virt = @import("./virt.zig");

pub const Address = packed struct(u32) {
    segment: u16,
    bus: u8,
    device: u5,
    function: u3,

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try std.fmt.format(
            writer,
            "[{X:0>4}:{X:0>2}:{X:0>2}.{X:0>1}]",
            .{ self.segment, self.bus, self.device, self.function },
        );
    }
};

const Segment = struct {
    segment: u16,
    base_address: u64,
    start_bus: u8,
    end_bus: u8,
};

const ConfigSpaceIO = struct {
    fn selectField(address: Address, offset: u32) void {
        std.debug.assert((offset & 0x3) == 0);
        std.debug.assert(address.segment == 0);
        arch.out(u32, 0xCF8, (1 << 31) |
            @as(u32, address.bus) << 16 |
            @as(u32, address.device) << 11 |
            @as(u32, address.function) << 8 |
            offset);
    }

    fn read(address: Address, offset: u32) u32 {
        selectField(address, offset);
        return arch.in(u32, 0xCFC);
    }

    fn write(address: Address, offset: u32, value: u32) void {
        selectField(address, offset);
        arch.out(u32, 0xCFC, value);
    }
};

const ConfigSpaceECAM = struct {
    fn getMMIOConfigSpace(address: Address) [*]volatile u32 {
        const segment = pci_segments[address.segment].?;
        std.debug.assert(address.bus >= segment.start_bus and address.bus <= segment.end_bus);
        return virt.asHigherHalfUncached(
            [*]volatile u32,
            segment.base_address | @as(u64, address.bus) << 20 |
                @as(u64, address.device) << 15 | @as(u64, address.function) << 12,
        );
    }

    fn read(address: Address, offset: u32) u32 {
        std.debug.assert(offset & 0x3 == 0);
        return getMMIOConfigSpace(address)[offset / 4];
    }

    fn write(address: Address, offset: u32, value: u32) void {
        std.debug.assert(offset & 0x3 == 0);
        getMMIOConfigSpace(address)[offset / 4] = value;
    }
};

var pci_segments: [16]?Segment = [1]?Segment{null} ** 16;
var read_config_space_fn: *const fn (Address, u32) u32 = ConfigSpaceIO.read;
var write_config_space_fn: *const fn (Address, u32, u32) void = ConfigSpaceIO.write;

pub fn readConfigSpace(comptime T: type, address: Address, offset: u32) T {
    const misalignment = offset & 0x3;
    const value = read_config_space_fn(address, offset & ~@as(u32, 0x3));
    std.debug.assert(misalignment + @sizeOf(T) <= 4);
    return @intCast((value >> @intCast(misalignment * 8)) & std.math.maxInt(T));
}

pub fn writeConfigSpace(comptime T: type, address: Address, offset: u32, value: T) void {
    const misalignment = offset & 0x3;
    std.debug.assert(misalignment + @sizeOf(T) <= 4);
    if (T == u32) {
        write_config_space_fn(address, offset, @as(u32, value));
        return;
    }
    const aligned_offset = offset & ~@as(u32, 0x3);
    const current = read_config_space_fn(address, aligned_offset);
    const new_value = current & (@as(u32, std.math.maxInt(T)) << @intCast(misalignment * 8)) |
        (@as(u32, value) << @intCast(misalignment * 8));
    write_config_space_fn(address, aligned_offset, new_value);
}

const DeviceList = std.TailQueue(void);

pub const BarInfo = struct {
    base_address: u64,
    length: usize,
    is_mmio: bool,
    is_64bit: bool,
    is_prefetchable: bool,
};

pub const Capability = struct {
    device: *const Device,
    offset: u32,
    id: u8,

    pub fn read(self: @This(), comptime T: type, offset: u32) T {
        return self.device.read(T, self.offset + offset);
    }

    pub fn write(self: @This(), comptime T: type, offset: u32, value: T) void {
        self.device.write(T, self.offset + offset, value);
    }
};

pub const MSI = struct {
    capability: Capability,
    is_msix: bool,

    pub fn enable(self: @This(), lapic_id: u32, vector: u8) void {
        const msi_addr: u64 = 0xFEE00000 | (lapic_id << 12);
        const msi_data = @as(u16, vector); // | (1 << 15); // Level triggered
        if (self.is_msix) {
            logger.info("Enabling MSI-X for device, lapic_id: {}, vector: {}", .{ lapic_id, vector });
            const msi_table_info = self.capability.read(u32, 0x4);
            const msi_table_bar = self.capability.device.bars[msi_table_info & 0x7];
            const msi_table = virt.asHigherHalfUncached(
                [*]volatile u32,
                msi_table_bar.?.base_address + (msi_table_info & 0xFFFFFFF8),
            );
            msi_table[3] = 0;
            msi_table[2] = msi_data;
            msi_table[0] = @truncate(msi_addr);
            msi_table[1] = @truncate(msi_addr >> 32);
            // Set the enable bit in the message control register
            const message_control = self.capability.read(u16, 0x2);
            self.capability.write(u16, 0x2, message_control | (1 << 15));
        } else {
            logger.info("Enabling MSI for device, lapic_id: {}, vector: {}", .{ lapic_id, vector });
            self.capability.write(u32, 0x4, @truncate(msi_addr));
            const message_control = self.capability.read(u16, 0x2);
            // 64-bit
            if (message_control & (1 << 7) != 0) {
                self.capability.write(u32, 0x8, @as(u32, @intCast(msi_addr >> 32)));
                self.capability.write(u16, 0xC, msi_data);
            } else {
                self.capability.write(u16, 0x8, msi_data);
            }
            // Set the enable bit in the message control register
            self.capability.write(u16, 0x2, message_control | (1 << 0));
        }

        // Clear the interrupt disable bit in the command register
        const command_register = self.capability.device.read(u32, 0x4);
        self.capability.device.write(u32, 0x4, command_register & ~@as(u16, 1 << 10));
    }
};

pub const Device = struct {
    node: DeviceList.Node = .{ .data = {} },
    address: Address,
    parent: ?*Device,
    bars: [6]?BarInfo,
    capabilities: std.BoundedArray(Capability, 32) = .{},
    msis: std.BoundedArray(MSI, 32) = .{},
    vendor_id: u16,
    device_id: u16,
    class_id: u8,
    subclass_id: u8,
    prog_if: u8,
    is_multifunction: bool,

    pub fn read(self: @This(), comptime T: type, offset: u32) T {
        return readConfigSpace(T, self.address, offset);
    }

    pub fn write(self: @This(), comptime T: type, offset: u32, value: T) void {
        writeConfigSpace(T, self.address, offset, value);
    }

    pub fn getMSI(self: @This(), prefer_msix: bool) ?MSI {
        var result: ?MSI = null;
        for (self.msis.constSlice()) |it| {
            if (it.is_msix == prefer_msix) {
                return it;
            }
        }
        if (result == null and self.msis.len > 0) {
            result = self.msis.get(0);
        }
        return result;
    }
};

var devices: DeviceList = .{};

fn checkFunction(address: Address, parent: ?*Device) !?*Device {
    const dword0 = readConfigSpace(u32, address, 0x0);
    // Check if the device is present by checking the vendor ID
    if ((dword0 & 0xFFFF) == 0xFFFF) {
        return null;
    }

    const dword1 = readConfigSpace(u32, address, 0x4);
    const dword2 = readConfigSpace(u32, address, 0x8);
    const dword3 = readConfigSpace(u32, address, 0xC);
    const device = try root.allocator.create(Device);
    device.* = .{
        .address = address,
        .parent = parent,
        .bars = [1]?BarInfo{null} ** 6,
        .vendor_id = @truncate(dword0),
        .device_id = @intCast(dword0 >> 16),
        .class_id = @intCast(dword2 >> 24),
        .subclass_id = @truncate(dword2 >> 16),
        .prog_if = @truncate(dword2 >> 8),
        .is_multifunction = ((dword3 >> 16) & (1 << 7)) != 0,
    };

    logger.info(
        "Found device {X:0>4}:{X:0>4} at {any}",
        .{ device.vendor_id, device.device_id, device.address },
    );

    // Find all BARs
    var last_bar_was_64bit = false;
    for (&device.bars, 0..) |*bar_info, i| {
        if (last_bar_was_64bit) {
            last_bar_was_64bit = false;
            continue;
        }

        const bar_offset: u32 = 0x10 + @as(u32, @intCast(i)) * 4;
        const bar = device.read(u32, bar_offset);
        const is_mmio = (bar & 0x1) == 0;
        if (is_mmio) {
            var base_address: u64 = bar & 0xFFFFFFF0;
            const is_64bit = ((bar >> 1) & 0x3) == 0x2;
            if (is_64bit) {
                const bar_high = device.read(u32, bar_offset + 4);
                base_address |= @as(u64, bar_high) << 32;
            }
            device.write(u32, bar_offset, 0xFFFFFFFF);
            const ones_out = device.read(u32, bar_offset) & 0xFFFFFFF0;
            const length = ~ones_out +% 1;
            bar_info.* = .{
                .base_address = base_address,
                .length = length,
                .is_mmio = true,
                .is_64bit = is_64bit,
                .is_prefetchable = (bar & (1 << 3)) != 0,
            };
        } else {
            const base_address = bar & 0xFFFFFFFC;
            device.write(u32, bar_offset, 0xFFFFFFFF);
            const ones_out = device.read(u32, bar_offset) & 0xFFFFFFFC;
            bar_info.* = .{
                .base_address = base_address,
                .length = ~ones_out +% 1,
                .is_mmio = false,
                .is_64bit = false,
                .is_prefetchable = false,
            };
        }

        device.write(u32, bar_offset, bar);
    }

    // Iterate the capability list
    if (((dword1 >> 16) & (1 << 4)) != 0) {
        var offset: u32 = device.read(u32, 0x34) & 0xFC;
        while (offset != 0) {
            const pci_word = device.read(u16, offset);
            const capability = try device.capabilities.addOne();
            capability.* = .{ .device = device, .offset = offset, .id = @intCast(pci_word & 0xFF) };
            offset = pci_word >> 8;
        }
    }

    // Collect MSI capabilities
    for (device.capabilities.constSlice()) |cap| {
        switch (cap.id) {
            0x5 => {
                const msi = try device.msis.addOne();
                msi.* = .{ .capability = cap, .is_msix = false };
            },
            0x11 => {
                const msi = try device.msis.addOne();
                msi.* = .{ .capability = cap, .is_msix = true };
            },
            else => {},
        }
    }

    devices.append(&device.node);

    inline for (drivers.pci_drivers) |driver| {
        switch (@as(drivers.PCIDriverDiscovery, driver.discovery)) {
            .all => try driver.handler(device),
            .id => |id| if (device.vendor_id == id.vendor and device.device_id == id.device) {
                try driver.handler(device);
            },
            .class => |class| if (device.class_id == class.class_id and device.subclass_id == class.subclass_id) {
                if (class.prog_if == null or device.prog_if == class.prog_if.?) {
                    try driver.handler(device);
                }
            },
        }
    }

    return device;
}

fn checkDevice(segment: u16, bus: u8, device_id: u5) !void {
    var address: Address = .{ .segment = segment, .bus = bus, .device = device_id, .function = 0 };
    const device = try checkFunction(address, null) orelse return;
    if (device.is_multifunction) {
        for (1..8) |function| {
            address.function = @intCast(function);
            _ = try checkFunction(address, device);
        }
    }
}

pub fn scanBus(segment: u16, bus: u8) !void {
    for (0..32) |i| {
        try checkDevice(segment, bus, @intCast(i));
    }
}

pub fn init() !void {
    const mcfg_table = acpi.findTable(uacpi.ACPI_MCFG_SIGNATURE) orelse {
        logger.err("Could not find the MCFG table", .{});
        return error.TableNotFound;
    };

    read_config_space_fn = ConfigSpaceECAM.read;
    write_config_space_fn = ConfigSpaceECAM.write;

    const hdr = mcfg_table.uacpi_table.unnamed_0.hdr;
    const mcfg: *uacpi.acpi_mcfg = @ptrCast(hdr);
    const entry_count = @divExact(hdr.*.length - @sizeOf(uacpi.acpi_mcfg), @sizeOf(uacpi.struct_acpi_mcfg_allocation));
    for (mcfg.entries()[0..entry_count]) |entry| {
        pci_segments[entry.segment] = .{
            .base_address = entry.address,
            .segment = entry.segment,
            .start_bus = entry.start_bus,
            .end_bus = entry.end_bus,
        };
        logger.info(
            "Found PCI segment {X:0>4} with base address 0x{X} and bus range {d}-{d}",
            .{ entry.segment, entry.address, entry.start_bus, entry.end_bus },
        );
    }
}
