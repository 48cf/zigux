const logger = std.log.scoped(.acpi);

const limine = @import("limine");
const root = @import("root");
const std = @import("std");

const apic = @import("./apic.zig");
const arch = @import("./arch.zig");
const virt = @import("./virt.zig");
const pci = @import("./pci.zig");
const uacpi = @import("./uacpi.zig");
const ps2 = @import("./drivers/ps2.zig");

pub const AcpiTable = struct {
    uacpi_table: *uacpi.uacpi_table,

    pub fn signature(self: *const @This()) [4]u8 {
        return self.uacpi_table.unnamed_0.hdr[0].signature;
    }

    pub fn getData(self: *const @This()) []const u8 {
        const hdr: *uacpi.acpi_sdt_hdr = @ptrCast(self.uacpi_table.unnamed_0.hdr);
        const data: [*]const u8 = @ptrCast(hdr);
        return data[0..hdr.length];
    }
};

pub fn findTable(signature: *const [4]u8) ?AcpiTable {
    var table: ?*uacpi.uacpi_table = null;
    if (uacpi.uacpi_table_find_by_signature(signature, &table) != uacpi.UACPI_STATUS_OK) {
        return null;
    }
    return .{ .uacpi_table = table.? };
}

pub fn init(rsdp_res: *limine.RsdpResponse) !void {
    try uacpi.initTables(rsdp_res);

    const madt_table = findTable(uacpi.ACPI_MADT_SIGNATURE) orelse {
        logger.err("Could not find the APIC table", .{});
        return error.TableNotFound;
    };

    var data = madt_table.getData()[@sizeOf(uacpi.acpi_madt)..];
    while (data.len >= @sizeOf(uacpi.acpi_entry_hdr)) {
        const entry: *const uacpi.acpi_entry_hdr = @ptrCast(data.ptr);
        switch (entry.type) {
            uacpi.ACPI_MADT_ENTRY_TYPE_IOAPIC => {
                const ioapic: *const uacpi.acpi_madt_ioapic = @ptrCast(entry);
                apic.handleIOAPIC(ioapic.address, ioapic.gsi_base);
            },
            uacpi.ACPI_MADT_ENTRY_TYPE_INTERRUPT_SOURCE_OVERRIDE => {
                const iso: *const uacpi.acpi_madt_interrupt_source_override = @ptrCast(entry);
                apic.handleIOAPICISO(iso.bus, iso.source, iso.gsi, iso.flags);
            },
            else => logger.warn("Unhandled MADT entry {d} of length {d}", .{ entry.type, entry.length }),
        }
        data = data[entry.length..];
    }

    try uacpi.init();
}

pub fn enumerateDevices() !void {
    const sb = uacpi.uacpi_namespace_get_predefined(uacpi.UACPI_PREDEFINED_NAMESPACE_SB);
    if (sb == null) {
        logger.err("Failed to get predefined namespace SB", .{});
        return error.NamespaceNotFound;
    }

    uacpi.uacpi_namespace_for_each_node_depth_first(sb, iterationCallback, null);
}

fn handlePciHostBridge(node: ?*uacpi.uacpi_namespace_node) void {
    const path = uacpi.uacpi_namespace_node_generate_absolute_path(node);
    defer uacpi.uacpi_free_absolute_path(path);

    var status: uacpi.uacpi_status = undefined;
    var segment: u64 = 0;
    var bus: u64 = 0;

    status = uacpi.uacpi_eval_integer(node, "_SEG", null, &segment);
    if (status != uacpi.UACPI_STATUS_OK and status != uacpi.UACPI_STATUS_NOT_FOUND) {
        logger.err("Failed to evaluate _SEG for {s}: {s}", .{ path, uacpi.uacpi_status_to_string(status) });
        return;
    }

    status = uacpi.uacpi_eval_integer(node, "_BBN", null, &bus);
    if (status != uacpi.UACPI_STATUS_OK and status != uacpi.UACPI_STATUS_NOT_FOUND) {
        logger.err("Failed to evaluate _BBN for {s}: {s}", .{ path, uacpi.uacpi_status_to_string(status) });
        return;
    }

    logger.info("Found PCI(e) host bridge {s}, segment {d}, bus {d}", .{ path, segment, bus });
    pci.scanBus(@intCast(segment), @intCast(bus)) catch |err| {
        logger.err("An error occurred during host bridge scan {s}: {any}", .{ path, err });
    };
}

fn iterationCallback(
    _: ?*anyopaque,
    node: ?*uacpi.uacpi_namespace_node,
) callconv(.C) uacpi.uacpi_ns_iteration_decision {
    const path = uacpi.uacpi_namespace_node_generate_absolute_path(node);
    defer uacpi.uacpi_free_absolute_path(path);

    var node_info: ?*uacpi.x_uacpi_namespace_node_info = null;
    if (uacpi.uacpi_get_namespace_node_info(node, @ptrCast(&node_info)) != uacpi.UACPI_STATUS_OK) {
        logger.warn("Failed to get information for node {s}", .{path});
        return uacpi.UACPI_NS_ITERATION_DECISION_CONTINUE;
    }

    const info = node_info.?;
    for (info.cid.ids()[0..info.cid.num_ids]) |cid_str| {
        const cid = cid_str.value[0 .. cid_str.size - 1];
        if (std.mem.eql(u8, cid, "PNP0A03") or std.mem.eql(u8, cid, "PNP0A08")) {
            handlePciHostBridge(node);
        } else if (std.mem.eql(u8, cid, "PNP0303")) {
            ps2.init(node) catch |err| {
                logger.err("An error occurred during PS/2 keyboard initialization: {any}", .{err});
            };
        }
    }

    return uacpi.UACPI_NS_ITERATION_DECISION_CONTINUE;
}
