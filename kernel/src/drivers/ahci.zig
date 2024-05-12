const logger = std.log.scoped(.ahci);

const root = @import("root");
const std = @import("std");

const arch = @import("../arch.zig");
const apic = @import("../apic.zig");
const pci = @import("../pci.zig");
const phys = @import("../phys.zig");
const dev_fs = @import("../vfs/dev_fs.zig");
const interrupts = @import("../interrupts.zig");
const scheduler = @import("../scheduler.zig");
const virt = @import("../virt.zig");
const utils = @import("../utils.zig");
const bf = @import("../containers/bit_fields.zig");

pub const pci_driver = .{
    .handler = handleDevice,
    .discovery = .{
        .class = .{
            .class_id = 1,
            .subclass_id = 6,
            .prog_if = 1,
        },
    },
};

fn read_u64(mmio: anytype) u64 {
    return @as(u64, mmio[0]) | (@as(u64, mmio[1]) << 32);
}

fn write_u64(mmio: anytype, value: u64) void {
    mmio[0] = @as(u32, @truncate(value));
    mmio[1] = @as(u32, @truncate(value >> 32));
}

const Prd = packed struct {
    data_base_addr: [2]u32,
    reserved_08: u32,
    sizem1: u22,
    reserved_10_22: u9,
    completion_interrupt: u1,
};

const FisH2D = packed struct {
    fis_type: u8,
    pmport: u4,
    reserved_1_4: u3,
    c: u1,

    command: u8,
    feature_low: u8,

    lba_low: u24,
    device: u8,

    lba_high: u24,
    feature_high: u8,

    count: u16,
    icc: u8,
    control: u8,

    reserved_10: u32,
};

comptime {
    std.debug.assert(@sizeOf(FisH2D) == 0x14);
}

const CommandFis = extern union {
    bytes: [0x40]u8,
    h2d: FisH2D,
};

const CommandTable = extern struct {
    command_fis: CommandFis,
    atapi_command: [0x10]u8,
    reserved_50: [0x80 - 0x50]u8,
    prds: [8]Prd,
};

const CommandTableHeader = packed struct {
    command_fis_length: u5,
    atapi: u1,
    write: u1,
    prefetchable: u1,

    sata_reset_control: u1,
    bist: u1,
    clear: u1,
    reserved1_3: u1,
    pmp: u4,

    pdrt_count: u16,
    command_table_byte_size: u32,
    command_table_addr: [2]u32,
    reserved: [4]u32,

    pub fn table(self: *volatile CommandTableHeader) *volatile CommandTable {
        const addr = read_u64(&self.command_table_addr);

        return virt.asHigherHalfUncached(*volatile CommandTable, addr);
    }
};

const CommandList = struct {
    command_headers: [32]CommandTableHeader,
};

const RecvFis = struct {
    dma_setup: [0x1C]u8,
    reserved_1C: [0x20 - 0x1C]u8,
    pio_setup: [0x14]u8,
    reserved_34: [0x40 - 0x34]u8,
    d2h_register: [0x14]u8,
    reserved_54: [0x58 - 0x54]u8,
    set_device_bits: [8]u8,
    unknown_fis: [0x40]u8,
    reserved_A0: [0x100 - 0xA0]u8,
};

const Port = extern struct {
    command_list_base: [2]u32,
    fis_base: [2]u32,
    interrupt_status: u32,
    interrupt_enable: u32,
    command_status: extern union {
        raw: u32,

        start: bf.Boolean(u32, 0),
        recv_enable: bf.Boolean(u32, 4),
        fis_recv_running: bf.Boolean(u32, 14),
        command_list_running: bf.Boolean(u32, 15),
    },
    reserved_0x1C: u32,
    task_file_data: extern union {
        raw: u32,

        transfer_requested: bf.Boolean(u32, 3),
        interface_busy: bf.Boolean(u32, 7),
    },
    signature: u32,
    sata_status: u32,
    sata_control: u32,
    sata_error: u32,
    sata_active: u32,
    command_issue: u32,
    sata_notification: u32,
    fis_switching_control: u32,
    device_sleep: u32,
    reserved_0x48: [0x70 - 0x48]u8,
    vendor_0x70: [0x80 - 0x70]u8,

    fn startCommandEngine(self: *volatile Port) void {
        logger.debug("0x{X}: Starting command engine", .{@intFromPtr(self)});

        self.waitReady();
        self.command_status.start.write(false);
        self.command_status.recv_enable.write(false);

        const status = &self.command_status;

        while (status.command_list_running.read() or status.fis_recv_running.read()) {
            scheduler.yield();
        }

        self.command_status.recv_enable.write(true);
        self.command_status.start.write(true);
    }

    fn stopCommandEngine(self: *volatile Port) void {
        logger.debug("0x{X}: Stopping command engine", .{@intFromPtr(self)});

        self.command_status.start.write(false);

        while (self.command_status.command_list_running.read()) {
            scheduler.yield();
        }

        self.command_status.recv_enable.write(false);

        while (self.command_status.fis_recv_running.read()) {
            scheduler.yield();
        }
    }

    fn waitReady(self: *volatile Port) void {
        const task_file_data = &self.task_file_data;

        while (task_file_data.transfer_requested.read() or task_file_data.interface_busy.read()) {
            scheduler.yield();
        }
    }

    fn getCommandHeaders(self: *const volatile Port) *volatile [32]CommandTableHeader {
        const addr = read_u64(&self.command_list_base);
        const cmd_list = virt.asHigherHalfUncached(*volatile CommandList, addr);

        return &cmd_list.command_headers;
    }

    fn getCommandHeader(self: *const volatile Port, slot: u5) *volatile CommandTableHeader {
        return &self.getCommandHeaders()[slot];
    }

    fn getCommandTable(self: *const volatile Port, slot: u5) *volatile CommandTable {
        return self.getCommandHeader(slot).table();
    }

    fn getFis(self: *volatile Port, slot: u5) *volatile CommandFis {
        return &self.getCommandTable(slot).command_fis;
    }

    fn makeH2D(self: *volatile Port, slot: u5) *volatile FisH2D {
        const fis = &self.getFis(slot).h2d;

        fis.fis_type = 0x27;

        return fis;
    }

    fn getPrd(self: *volatile Port, slot: u5, index: usize) *volatile Prd {
        return &self.getCommandTable(slot).prds[index];
    }

    fn getBuffer(self: *volatile Port, slot: u5, index: usize) []u8 {
        const prd_ptr = self.getPrd(slot, index);
        const buf_addr = read_u64(&prd_ptr.data_base_addr);
        const buf_size = @as(usize, prd_ptr.sizem1) + 1;

        return virt.asHigherHalfUncached([*]u8, buf_addr)[0..buf_size];
    }

    fn issueCommands(self: *volatile Port, slot_bits: u32) void {
        logger.debug("0x{X}: Sending {d} command(s)", .{ @intFromPtr(self), @popCount(slot_bits) });

        self.waitReady();
        self.command_issue |= slot_bits;

        while (self.command_issue & slot_bits != 0) {
            scheduler.yield();
        }
    }
};

const Abar = extern struct {
    hba_capabilities: u32,
    global_hba_control: u32,
    interrupt_status: u32,
    ports_implemented: u32,
    version: extern union {
        value: u32,

        major: bf.BitField(u32, 16, 16),
        minor_high: bf.BitField(u32, 8, 8),
        minor_low: bf.BitField(u32, 0, 8),

        pub fn format(
            self: *const @This(),
            comptime fmt: []const u8,
            options: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            _ = fmt;
            _ = options;

            try writer.print("{d}.{d}", .{ self.major.read(), self.minor_high.read() });

            if (self.minor_low.read() != 0) {
                try writer.print(".{d}", .{self.minor_low.read()});
            }
        }
    },
    command_completion_coalescing_control: u32,
    command_completion_coalescing_port: u32,
    enclosure_managment_location: u32,
    enclosure_managment_control: u32,
    hba_capabilities_extended: u32,
    bios_handoff: extern union {
        value: u32,
        bios_owned: bf.Boolean(u32, 4),
        os_owned: bf.Boolean(u32, 1),
        bios_busy: bf.Boolean(u32, 0),

        fn setHandoff(self: *volatile @This()) void {
            self.os_owned.write(true);
        }

        fn checkHandoff(self: *volatile @This()) bool {
            if (self.bios_owned.read())
                return false;

            if (self.bios_busy.read())
                return false;

            if (self.os_owned.read())
                return true;

            return false;
        }

        fn tryTakeOver(self: *volatile @This()) bool {
            self.setHandoff();

            return self.checkHandoff();
        }
    },
    reserved_0x2C: u32,
    reserved_0x30: [0xA0 - 0x30]u8,
    vendor_0xA0: [0x100 - 0xA0]u8,
    ports: [32]Port,
};

const SataPortType = enum {
    Ata,
    Atapi,
};

const ReadOrWrite = enum {
    Read,
    Write,
};

const PortState = struct {
    mmio: *volatile Port,
    sector_size: usize,
    sector_count: usize,
    port_type: SataPortType,

    pub fn init(port: *volatile Port) !*PortState {
        const result = try root.allocator.create(PortState);

        errdefer root.allocator.destroy(result);

        result.* = .{
            .mmio = port,
            .sector_size = 0,
            .sector_count = 0,
            .port_type = switch (result.mmio.signature) {
                0x00000101 => .Ata,
                // 0xEB140101 => .Atapi, // Drop atapi for now
                else => return error.UnknownSignature,
            },
        };

        result.mmio.stopCommandEngine();
        try result.setupCommandHeaders();
        try result.setupPrdts();
        result.mmio.startCommandEngine();
        try result.identify();

        return result;
    }

    pub fn getSectorSize(self: *const PortState) usize {
        return self.sector_size;
    }

    pub fn getSectorCount(self: *const PortState) usize {
        return self.sector_count;
    }

    pub fn readBlock(self: *PortState, block_in: usize, buffer_in: []u8) !void {
        var block = block_in;
        var buffer = buffer_in;

        while (buffer.len > 0) {
            const blocks = utils.alignUp(usize, buffer.len, self.sector_size) / self.sector_size;
            const sector_count = @min(blocks, std.mem.page_size / self.sector_size);
            const copy_length = @min(sector_count * self.sector_size, buffer.len);
            const mmio_buffer = self.mmio.getBuffer(0, 0);

            self.finalizeIo(0, @as(u48, @intCast(block)), @as(u16, @intCast(sector_count)), .Read);

            @memcpy(buffer, mmio_buffer[0..copy_length]);

            block += sector_count;
            buffer = buffer[copy_length..];
        }
    }

    pub fn writeBlock(self: *PortState, block_in: usize, buffer_in: []const u8) !void {
        var block = block_in;
        var buffer = buffer_in;

        while (buffer.len > 0) {
            const blocks = utils.alignUp(usize, buffer.len, self.sector_size) / self.sector_size;
            const sector_count = @min(blocks, std.mem.page_size / self.sector_size);
            const copy_length = @min(sector_count * self.sector_size, buffer.len);
            const mmio_buffer = self.mmio.getBuffer(0, 0);

            @memcpy(mmio_buffer[0..copy_length], buffer);

            self.finalizeIo(0, @as(u48, @intCast(block)), @as(u16, @intCast(sector_count)), .Write);

            block += sector_count;
            buffer = buffer[copy_length..];
        }
    }

    fn setupCommandHeaders(self: *PortState) !void {
        const port_io_size = @sizeOf(CommandList) + @sizeOf(RecvFis);
        const page_count = std.mem.alignForward(u64, port_io_size, std.mem.page_size) / std.mem.page_size;
        const commands_phys = phys.allocate(page_count, .dma) orelse return error.OutOfMemory;
        const fis_phys = commands_phys + @sizeOf(CommandList);

        write_u64(&self.mmio.command_list_base, commands_phys);
        write_u64(&self.mmio.fis_base, fis_phys);
    }

    fn setupPrdts(self: *PortState) !void {
        var current_table_addr: usize = undefined;
        var remaining_table_size: usize = 0;

        for (self.mmio.getCommandHeaders()) |*header| {
            if (remaining_table_size < @sizeOf(CommandTable)) {
                remaining_table_size = std.mem.page_size;
                current_table_addr = phys.allocate(1, .dma) orelse return error.OutOfMemory;
            }

            write_u64(&header.command_table_addr, current_table_addr);

            current_table_addr += @sizeOf(CommandTable);
            remaining_table_size -= @sizeOf(CommandTable);

            header.pdrt_count = 1;
            header.command_fis_length = @sizeOf(FisH2D) / @sizeOf(u32);
            header.atapi = if (self.port_type == .Atapi) 1 else 0;

            const prd = &header.table().prds[0];
            const buf = phys.allocate(1, .dma) orelse return error.OutOfMemory;

            write_u64(&prd.data_base_addr, buf);

            prd.sizem1 = @as(u22, @intCast(std.mem.page_size - 1));
        }
    }

    fn identify(self: *PortState) !void {
        const identify_fis = self.mmio.makeH2D(0);

        identify_fis.command = if (self.port_type == .Atapi) 0xA1 else 0xEC;
        identify_fis.c = 1;
        identify_fis.device = 0xA0 | (1 << 6);
        identify_fis.control = 0x08;

        self.issueCommandOnSlot(0);

        const buffer = self.mmio.getBuffer(0, 0);
        const data_valid = std.mem.readIntLittle(u16, buffer[212..][0..2]);
        const read_sector_size = data_valid & (1 << 15) == 0 and data_valid & (1 << 14) != 0 and data_valid & (1 << 12) != 0;

        if (read_sector_size) {
            self.sector_size = std.mem.readIntLittle(u32, buffer[234..][0..4]);
        } else {
            self.sector_size = 512;
        }

        self.sector_count = std.mem.readIntLittle(u64, buffer[200..][0..8]);

        if (self.sector_count == 0) {
            self.sector_count = std.mem.readIntLittle(u32, buffer[120..][0..4]);
        }

        if (self.sector_count == 0) {
            return error.NoSectors;
        }

        const disk_size = self.sector_count * self.sector_size;

        for (std.mem.bytesAsSlice(u16, buffer[20..40])) |*value| {
            value.* = @byteSwap(value.*);
        }

        for (std.mem.bytesAsSlice(u16, buffer[54..94])) |*value| {
            value.* = @byteSwap(value.*);
        }

        const serial_str = std.mem.trimRight(u8, buffer[20..40], " ");
        const model_str = std.mem.trimRight(u8, buffer[54..94], " ");

        logger.info("0x{X}: Identified as {s} with serial {s}", .{ @intFromPtr(self.mmio), model_str, serial_str });
        logger.info(
            "0x{X}: Disk has 0x{X} sectors of size {d} ({} in total)",
            .{ @intFromPtr(self.mmio), self.sector_count, self.sector_size, utils.BinarySize.init(disk_size) },
        );

        try dev_fs.addDiskBlockDevice("sata", self);
    }

    fn finalizeIo(self: *PortState, command_slot: u5, lba: u48, sector_count: u16, mode: ReadOrWrite) void {
        const fis = self.mmio.makeH2D(0);

        fis.command = switch (self.port_type) {
            .Ata => switch (mode) {
                .Read => @as(u8, 0x25),
                .Write => 0x35,
            },
            else => unreachable,
        };

        fis.device = 0xA0 | (1 << 6);
        fis.control = 0x08;

        fis.lba_low = @as(u24, @truncate(lba));
        fis.lba_high = @as(u24, @truncate(lba >> 24));
        fis.count = sector_count;

        self.issueCommandOnSlot(command_slot);
    }

    fn issueCommandOnSlot(self: *PortState, command_slot: u5) void {
        self.mmio.issueCommands(@as(u32, 1) << command_slot);
    }
};

pub fn handleDevice(device: pci.Device) !void {
    logger.info(
        "Handling device {X:0>4}:{X:0>4}",
        .{ device.vendor_id().read(), device.device_id().read() },
    );

    const bar5 = device.getBar(5).?;
    const abar = virt.asHigherHalfUncached(*volatile Abar, bar5.base);

    if (abar.hba_capabilities & 1 << 31 == 0) {
        logger.warn("Controller is 32-bit only, ignoring", .{});
        return;
    }

    if (abar.global_hba_control & 1 << 31 == 0) {
        logger.warn("Controller is not in AHCI mode, ignoring", .{});
        return;
    }

    const vector = interrupts.allocateVector();
    const msi = device.getMsi().?;

    device.enableDma();
    interrupts.registerHandler(vector, interruptHandler);
    msi.enable(apic.localApicId(), vector);

    _ = try scheduler.startKernelThread(controllerThread, abar);
}

fn interruptHandler(frame: *interrupts.InterruptFrame) void {
    _ = frame;

    logger.debug("AHCI interrupt", .{});
}

fn takeOverController(abar: *volatile Abar) void {
    const version = abar.version;

    logger.debug("Controller version: {}", .{version});

    if (version.major.read() < 1 or version.minor_high.read() < 2) {
        logger.debug("Handoff not supported (version too old)", .{});
        return;
    }

    if (abar.hba_capabilities_extended & 1 == 0) {
        logger.debug("Handoff not supported", .{});
        return;
    }

    while (!abar.bios_handoff.tryTakeOver()) {
        scheduler.yield();
    }

    logger.debug("Handoff complete", .{});
}

fn initializePort(port: *volatile Port) !void {
    _ = try PortState.init(port);
}

fn controllerThread(abar: *volatile Abar) !void {
    takeOverController(abar);

    const ports_implemented = abar.ports_implemented;

    for (abar.ports, 0..) |*port, i| {
        if ((ports_implemented >> @as(u5, @intCast(i))) & 1 == 0) {
            continue;
        }

        const sata_status = port.sata_status;
        const com_status = sata_status & 0xF;
        const ipm_status = (sata_status >> 8) & 0xF;

        if (com_status == 0) {
            continue;
        }

        if (com_status != 3) {
            logger.warn("({d}) Unknown port status: {X}", .{ i, com_status });
            continue;
        }

        if (ipm_status != 1) {
            logger.warn("({d}) Device sleeping: {X}", .{ i, ipm_status });
            continue;
        }

        switch (port.signature) {
            0x00000101 => _ = try scheduler.startKernelThread(initializePort, port),
            0xEB140101 => continue, // ATAPI isn't supported yet
            else => {
                logger.warn("({d}) Unknown AHCI port signature: 0x{X}", .{ i, port.signature });
                continue;
            },
        }
    }
}
