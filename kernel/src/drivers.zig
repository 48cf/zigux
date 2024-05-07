const pci = @import("pci.zig");

pub const pci_drivers = .{
    // @import("drivers/ahci.zig").pci_driver,
    @import("drivers/xhci.zig").pci_driver,
};

pub const PciDriverDiscovery = union(enum) {
    all,
    id: struct {
        vendor: u16,
        device: u16,
    },
    class: struct {
        class_id: u8,
        subclass_id: u8,
        prog_if: ?u8,
    },
};
