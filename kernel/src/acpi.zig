const logger = std.log.scoped(.acpi);

const std = @import("std");
const limine = @import("limine");

const apic = @import("apic.zig");
const virt = @import("virt.zig");

const Rsdp = extern struct {
    signature: [8]u8,
    checksum: u8,
    oem_id: [6]u8,
    revision: u8,
    rsdt_address: u32,
    length: u32,
    xsdt_address: u64,
    extended_checksum: u8,
    reserved: [3]u8,
};

const Sdt = extern struct {
    signature: [4]u8,
    length: u32,
    revision: u8,
    checksum: u8,
    oem_id: [6]u8,
    oem_table_id: [8]u8,
    oem_revision: u32,
    creator_id: u32,
    creator_revision: u32,

    fn getData(self: SdtPtr) []const u8 {
        return @ptrCast([*]const u8, self)[0..self.length][@sizeOf(Sdt)..];
    }
};

const RsdpPtr = *align(1) const Rsdp;
const SdtPtr = *align(1) const Sdt;

fn signatureValue(sdt: anytype) u32 {
    return std.mem.readIntNative(u32, sdt[0..4]);
}

fn handleTable(sdt: SdtPtr) !void {
    switch (signatureValue(sdt.signature)) {
        signatureValue("APIC") => {
            var data = sdt.getData()[8..];

            while (data.len >= 2) {
                const kind = data[0];
                const size = data[1];

                if (size >= data.len)
                    break;

                const record_data = data[2..size];

                switch (kind) {
                    0 => {}, // logger.debug("Unhandled LAPIC: {any}", .{record_data}),
                    1 => apic.handleIoApic(
                        record_data[0],
                        std.mem.readIntNative(u32, record_data[2..6]),
                        std.mem.readIntNative(u32, record_data[6..10]),
                    ),
                    2 => apic.handleIoApicIso(
                        record_data[0],
                        record_data[1],
                        std.mem.readIntNative(u32, record_data[2..6]),
                        std.mem.readIntNative(u16, record_data[6..8]),
                    ),
                    // 3 => logger.debug("Unhandled IO/APIC NMI source: {any}", .{record_data}),
                    // 4 => logger.debug("Unhandled LAPIC NMI: {any}", .{record_data}),
                    // 5 => logger.debug("Unhandled LAPIC Address Override: {any}", .{record_data}),
                    // 9 => logger.debug("Unhandled x2LAPIC: {any}", .{record_data}),
                    else => logger.warn("Unknown MADT record 0x{X}: {any}", .{ kind, record_data }),
                }

                data = data[std.math.max(2, size)..];
            }
        },
        else => {}, // logger.debug("Unhandled ACPI table: {s}", .{sdt.signature}),
    }
}

fn parse(comptime T: type, bytes: []const u8) !void {
    const entries = std.mem.bytesAsSlice(T, bytes);

    for (entries) |entry| {
        try handleTable(virt.asHigherHalf(SdtPtr, entry));
    }
}

pub fn init(rsdp_res: *limine.RsdpResponse) !void {
    const rsdp = @ptrCast(RsdpPtr, rsdp_res.address);

    switch (rsdp.revision) {
        0 => try parse(u32, virt.asHigherHalf(SdtPtr, rsdp.rsdt_address).getData()),
        2 => try parse(u64, virt.asHigherHalf(SdtPtr, rsdp.xsdt_address).getData()),
        else => std.debug.panic("Unknown ACPI revision: {}", .{rsdp.revision}),
    }
}
