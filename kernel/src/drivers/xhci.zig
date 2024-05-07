const logger = std.log.scoped(.xhci);

const root = @import("root");
const std = @import("std");

const apic = @import("../apic.zig");
const arch = @import("../arch.zig");
const pci = @import("../pci.zig");
const virt = @import("../virt.zig");
const phys = @import("../phys.zig");
const scheduler = @import("../scheduler.zig");
const interrupts = @import("../interrupts.zig");

pub const pci_driver = .{
    .handler = handleDevice,
    .discovery = .{
        .class = .{
            .class_id = 0xc,
            .subclass_id = 0x3,
            .prog_if = 0x30,
        },
    },
};

const ReadQueue = struct {
    data: [*]volatile TransferRequestBlock,
    cycle_state: u32 = 1,
    offset: u32 = 0,

    fn dequeue(self: *@This()) ?*TransferRequestBlock {
        if (self.data[self.offset][3] & (1 << 0) == self.cycle_state) {
            const offset = self.offset;
            self.offset += 1;
            if (self.offset == trb_count) {
                self.cycle_state ^= 1;
                self.offset = 0;
            }
            return @volatileCast(&self.data[offset]);
        }

        return null;
    }
};

const WriteQueue = struct {
    data: [*]volatile TransferRequestBlock,
    dirty: bool = false,
    cycle_state: u32 = 1,
    offset: u32 = 0,

    fn enqueue(self: *@This(), a: u32, b: u32, c: u32, d: u32) *const TransferRequestBlock {
        std.debug.assert(d & 1 == 0);
        if (self.offset == trb_count - 1) {
            const data_phys = virt.higherHalfUncachedToPhysical(self.data);
            self.data[self.offset][0] = @truncate(data_phys);
            self.data[self.offset][1] = @truncate(data_phys >> 32);
            self.data[self.offset][3] = self.cycle_state | (1 << 1) | (6 << 10);
            self.cycle_state ^= 1;
            self.offset = 0;
        }

        logger.debug("Enqueueing TRB {X} {X} {X} {X}", .{ a, b, c, d });
        const trb: *volatile TransferRequestBlock = &self.data[self.offset];
        trb[0] = a;
        trb[1] = b;
        trb[2] = c;
        trb[3] = d | self.cycle_state;

        self.offset += 1;
        self.dirty = true;
        return @volatileCast(trb);
    }
};

const Port = struct {
    connected: bool,
    usb_version_major: u8,
    usb_version_minor: u8,
    port_speed: u8,

    slot_id: u8,
    enable_slot_trb: ?*const TransferRequestBlock,
};

const Slot = struct {
    input_context: *volatile anyopaque,
    device_context: *volatile anyopaque,

    descriptor_buffer: u64,

    ep0_queue: WriteQueue,
};

const Controller = struct {
    caps: *volatile CapabilityRegs,
    ops: *volatile OperationalRegs,
    runtime: *volatile RuntimeRegs,
    doorbells: [*]volatile u32,

    event_queue: ReadQueue,
    command_queue: WriteQueue,

    ports: std.BoundedArray(Port, 256),
    slots: std.BoundedArray(Slot, 256),

    // ...
    const portsc_keep = 0xC3E0;

    // PORTSC.CSC | PORTSC.PRC
    const portsc_events = (1 << 17) | (1 << 21);

    fn getInputContext(self: *@This(), comptime T: type, slot_id: usize, index: usize) *volatile T {
        std.debug.assert(index <= 32);
        const slot = self.slots.get(slot_id);
        return @ptrFromInt(@intFromPtr(slot.input_context) + index * self.caps.contextStride());
    }

    fn getDeviceContext(self: *@This(), comptime T: type, slot_id: usize, index: usize) *volatile T {
        std.debug.assert(index < 32);
        const slot = self.slots.get(slot_id);
        return @ptrFromInt(@intFromPtr(slot.device_context) + index * self.caps.contextStride());
    }

    fn sendControlTransfer(
        self: *@This(),
        slot_id: u8,
        endpoint: u8,
        request_type: u8,
        request: u8,
        value: u16,
        index: u16,
        length: u16,
        buffer: u64,
    ) void {
        std.debug.assert(endpoint == 0);
        const slot = &self.slots.slice()[slot_id];
        const has_data_stage = length > 0;
        const trt: u32 = if (has_data_stage) (request_type >> 7) | 0x2 else 0;
        _ = slot.ep0_queue.enqueue(
            @as(u32, request_type) | @as(u32, request) << 8 | @as(u32, value) << 16,
            @as(u32, index) | @as(u32, length) << 16,
            8,
            (1 << 6) | (2 << 10) | (trt << 16),
        );
        if (has_data_stage) {
            _ = slot.ep0_queue.enqueue(
                @truncate(buffer),
                @intCast(buffer >> 32),
                length,
                (1 << 2) | (3 << 10) | @as(u32, request_type >> 7) << 16,
            );
        }
        const status_dir = (1 << 5) | (4 << 10) | @as(u32, (request_type >> 7) ^ 1 | @intFromBool(!has_data_stage)) << 16;
        _ = slot.ep0_queue.enqueue(0, 0, 0, status_dir);
    }

    fn resetPort(self: *@This(), port_id: u8) void {
        const portsc: *volatile u32 = @ptrFromInt(@intFromPtr(self.ops) + 0x400 + @as(usize, port_id) * 0x10);
        const portsc_value = portsc.*;

        // Set the PORTSC.PRC bit
        portsc.* = (portsc_value & portsc_keep) | (1 << 4);
    }

    fn checkPortStatus(self: *@This(), port_id: u8) void {
        const port = &self.ports.slice()[port_id];
        const portsc: *volatile u32 = @ptrFromInt(@intFromPtr(self.ops) + 0x400 + @as(usize, port_id) * 0x10);
        const portsc_value = portsc.*;

        portsc.* = portsc_value & (portsc_keep | portsc_events);
        port.port_speed = @truncate((portsc_value >> 10) & 0xf);

        // Check for port connect status change without port reset
        if (portsc_value & portsc_events == (1 << 17)) {
            if (port.connected) {
                port.connected = false;
                _ = self.command_queue.enqueue(0, 0, 0, @as(u32, port.slot_id) << 24 | (10 << 10));
            }

            // If the port is a USB2 port, we need to reset it
            if (port.usb_version_major == 2) {
                self.resetPort(port_id);
                return;
            }
        }

        if (!port.connected and portsc_value & (1 << 0) != 0) {
            port.connected = true;
            port.enable_slot_trb = self.command_queue.enqueue(0, 0, 0, (9 << 10));
        }
    }
};

const CapabilityRegs = extern struct {
    caplength: u8, // 0x0
    hciversion: u16, // 0x2
    hcsparams1: u32, // 0x4
    hcsparams2: u32, // 0x8
    hcsparams3: u32, // 0xC
    hccparams1: u32, // 0x10
    dboff: u32, // 0x14
    rtsoff: u32, // 0x18
    hccparams2: u32, // 0x1C

    fn contextStride(self: *const volatile @This()) usize {
        return @as(usize, 32) << @as(u1, @truncate(self.hccparams1 >> 2));
    }
};

const OperationalRegs = extern struct {
    usbcmd: u32, // 0x0
    usbsts: u32, // 0x4
    pagesize: u32, // 0x8
    reserved1: [2]u32, // 0xC
    dnctrl: u32, // 0x14
    crcr: u64, // 0x18
    reserved2: [4]u32, // 0x20
    dcbaap: u64, // 0x30
    config: u32, // 0x38
};

const InterruptRegisterSet = extern struct {
    iman: u32,
    imod: u32,
    erstsz: u32,
    reserved: u32,
    erstba: u64,
    erdp: u64,
};

const RuntimeRegs = extern struct {
    mfindex: u32, // 0x0
    reserved: [7]u32, // 0x4
    ir: [1024]InterruptRegisterSet,
};

const SlotContext = extern struct {
    dw0: packed struct(u32) {
        route_string: u20,
        speed: u4,
        reserved: u1,
        multi_tt: u1,
        hub: u1,
        context_entries: u5,
    },
    dw1: packed struct(u32) {
        max_exit_latency: u16,
        root_hub_port_number: u8,
        number_of_ports: u8,
    },
    dw2: packed struct(u32) {
        hub_slot_id: u8,
        port_number: u8,
        tt_think_time: u2,
        reserved: u4,
        interrupter_target: u10,
    },
    dw3: packed struct(u32) {
        usb_device_address: u8,
        reserved: u19,
        slot_state: u5,
    },
};

const EndpointContext = extern struct {
    dw0: packed struct(u32) {
        ep_state: u3,
        reserved: u5,
        mult: u2,
        max_primary_streams: u5,
        linear_stream_array: u1,
        interval: u8,
        max_esit_payload_hi: u8,
    },
    dw1: packed struct(u32) {
        reserved1: u1,
        error_count: u2,
        ep_type: u3,
        reserved2: u1,
        hid: u1,
        max_burst_size: u8,
        max_packet_size: u16,
    },
    tr_dequeue_ptr: u64,
    dw4: packed struct(u32) {
        average_trb_length: u16,
        max_esit_payload: u16,
    },
    reserved: [3]u32,
};

const InputControlContext = extern struct {
    d: u32,
    a: u32,
    reserved: [5]u32,
    dw7: packed struct(u32) {
        configuration_value: u8,
        interface_number: u8,
        alternate_setting: u8,
        reserved: u8,
    },
};

const trb_count = std.mem.page_size / @sizeOf(TransferRequestBlock);

const TransferRequestBlock = [4]u32;

const EventRingSegmentTableEntry = extern struct {
    segment_base_address: u64,
    segment_size: u16,
    reserved: [3]u16,
};

fn interruptHandler(context: u64) void {
    defer apic.eoi();

    const controller: *Controller = @ptrFromInt(context);
    logger.debug("XHCI interrupt received", .{});

    while (controller.event_queue.dequeue()) |trb| {
        const trb_id = (trb[3] >> 10) & 0x3f;
        switch (trb_id) {
            // Transfer Event
            32 => {
                const completed_trb = virt.asHigherHalfUncached(*const TransferRequestBlock, @as(u64, trb[0]) | @as(u64, trb[1]) << 32);
                const completion_code: u8 = @truncate(trb[2] >> 24);
                if (completion_code != 1 and completion_code != 13) {
                    logger.err("Command completion failed with code {d}", .{completion_code});
                    logger.debug("TRB {X} {X} {X} {X}", .{ completed_trb[0], completed_trb[1], completed_trb[2], completed_trb[3] });
                    continue;
                } else if (completion_code == 13) {
                    logger.warn("Device sent a short packet, this is unhandled for now", .{});
                    continue;
                }

                logger.debug("Got transfer event", .{});

                const slot_id = @as(u8, @truncate(trb[3] >> 24)) - 1;
                const slot = &controller.slots.slice()[slot_id];

                logger.debug(
                    "Received decscriptor from slot {d}: {any}",
                    .{ slot_id, std.fmt.fmtSliceHexUpper(virt.asHigherHalf([*]u8, slot.descriptor_buffer)[0..8]) },
                );
                phys.free(slot.descriptor_buffer, 1);
            },
            // Command Completion Event
            33 => {
                const completed_trb = virt.asHigherHalfUncached(*const TransferRequestBlock, @as(u64, trb[0]) | @as(u64, trb[1]) << 32);
                const completion_code: u8 = @truncate(trb[2] >> 24);
                if (completion_code != 1) {
                    logger.err("Command completion failed with code {d}", .{completion_code});
                    logger.debug("TRB {X} {X} {X} {X}", .{ completed_trb[0], completed_trb[1], completed_trb[2], completed_trb[3] });
                    continue;
                }

                const completion_type = (completed_trb[3] >> 10) & 0x3f;
                switch (completion_type) {
                    // Enable Slot Command
                    9 => {
                        var port_number: ?u8 = 0;
                        for (controller.ports.slice(), 0..) |port, i| {
                            if (port.enable_slot_trb == completed_trb) {
                                port_number = @intCast(i);
                                break;
                            }
                        }

                        if (port_number) |port_num| {
                            const related_port = &controller.ports.slice()[port_num];
                            const slot_id = @as(u8, @truncate(trb[3] >> 24)) - 1;
                            related_port.enable_slot_trb = null;
                            related_port.slot_id = slot_id;
                            logger.debug("Allocated slot {d} for port {d}", .{ slot_id, port_num });

                            const slot = &controller.slots.slice()[slot_id];
                            const input_control_context = controller.getInputContext(InputControlContext, slot_id, 0);
                            input_control_context.a = (1 << 0) | (1 << 1);

                            const input_slot_context = controller.getInputContext(SlotContext, slot_id, 1);
                            input_slot_context.dw1.root_hub_port_number = port_num + 1;
                            input_slot_context.dw0.route_string = 0;
                            input_slot_context.dw0.context_entries = 1;

                            const input_ep0_context = controller.getInputContext(EndpointContext, slot_id, 2);
                            input_ep0_context.dw1.ep_type = 4; // Control Endpoint
                            input_ep0_context.dw1.max_packet_size = switch (related_port.port_speed) {
                                2 => 8, // Low Speed,
                                1, 3 => 64, // Full Speed/High Speed,
                                4, 5 => 512, // Super Speed/ Super Speed Plus,
                                else => unreachable,
                            };
                            input_ep0_context.dw1.max_burst_size = 0;
                            input_ep0_context.tr_dequeue_ptr = virt.higherHalfUncachedToPhysical(slot.ep0_queue.data) | slot.ep0_queue.cycle_state;
                            input_ep0_context.dw0.interval = 0;
                            input_ep0_context.dw0.max_primary_streams = 0;
                            input_ep0_context.dw0.mult = 0;
                            input_ep0_context.dw1.error_count = 3;

                            const input_context_phys = virt.higherHalfUncachedToPhysical(slot.input_context);
                            _ = controller.command_queue.enqueue(
                                @truncate(input_context_phys),
                                @intCast(input_context_phys >> 32),
                                0,
                                @as(u32, slot_id + 1) << 24 | (11 << 10),
                            );
                            logger.debug("Enqueued address device command for slot {d}", .{slot_id});
                        } else {
                            logger.err("Failed to find related port for enable slot command", .{});
                        }
                    },
                    // Address Device Command
                    11 => {
                        logger.debug("Addressed device", .{});

                        // const slot_id = @as(u8, @truncate(trb[3] >> 24)) - 1;
                        // const slot = &controller.slots.slice()[slot_id];

                        // slot.descriptor_buffer = phys.allocate(1, false).?;
                        // controller.sendControlTransfer(slot_id, 0, 0x80, 6, 1 << 8, 0, 8, slot.descriptor_buffer);
                    },
                    else => {
                        logger.debug("Got completion event for unknown command {d}", .{completion_type});
                    },
                }
            },
            // Port Status Change Event
            34 => {
                const port_id = @as(u8, @truncate(trb[0] >> 24)) - 1;
                controller.checkPortStatus(port_id);
            },
            else => {
                logger.debug("Got TRB {X} {X} {X} {X} {X}", .{ trb_id, trb[0], trb[1], trb[2], trb[3] });
            },
        }
    }

    // Signal to the controller the last TRB that was processed
    const next_trb = &controller.event_queue.data[controller.event_queue.offset];
    controller.runtime.ir[0].erdp = virt.higherHalfUncachedToPhysical(next_trb) | (1 << 3);

    // Clear the IMAN.IP flag
    controller.runtime.ir[0].iman |= (1 << 0);

    if (controller.command_queue.dirty) {
        controller.command_queue.dirty = false;
        controller.doorbells[0] = 0;
    }

    for (controller.slots.slice(), 0..) |*slot, i| {
        if (slot.ep0_queue.dirty) {
            slot.ep0_queue.dirty = false;
            controller.doorbells[i + 1] = 1;
        }
    }
}

fn controllerThread(param: u32) !void {
    const address: PciAddress = @bitCast(param);
    const device = pci.Device{ .bus = address.bus, .slot = address.slot, .function = address.function };

    const bar0 = device.getBar(0).?;
    const caps = virt.asHigherHalfUncached(*volatile CapabilityRegs, bar0.base);

    const controller = try root.allocator.create(Controller);
    controller.* = Controller{
        .caps = caps,
        .ops = virt.asHigherHalfUncached(*volatile OperationalRegs, bar0.base + caps.caplength),
        .runtime = virt.asHigherHalfUncached(*volatile RuntimeRegs, bar0.base + caps.rtsoff),
        .doorbells = virt.asHigherHalfUncached([*]volatile u32, bar0.base + caps.dboff),

        .command_queue = .{ .data = undefined },
        .event_queue = .{ .data = undefined },

        .ports = std.BoundedArray(Port, 256).init(@as(u8, @truncate(caps.hcsparams1 >> 24))) catch unreachable,
        .slots = std.BoundedArray(Slot, 256).init(@as(u8, @truncate(caps.hcsparams1))) catch unreachable,
    };

    // Make sure the controller supports 4KiB pages
    if (controller.ops.pagesize & (1 << 0) != 1) {
        logger.err("Controller does not support 4KiB pages", .{});
        return error.Unsupported;
    }

    // Perform Chip Hardware Reset

    // Stop the controller by clearing bit USBCMD.RS
    controller.ops.usbcmd &= ~@as(u32, 1 << 0);

    // Wait for the controller to halt
    while (controller.ops.usbsts & (1 << 0) != 1) {
        scheduler.yield();
    }

    // Reset the controller by setting bit USBCMD.HCRST
    logger.debug("Resetting controller", .{});
    controller.ops.usbcmd |= (1 << 1);

    // Wait for USBSTS.CNR to be cleared
    while (controller.ops.usbsts & (1 << 11) != 0) {
        scheduler.yield();
    }

    logger.debug("Max controller slots: {d}", .{controller.slots.len});
    logger.debug("Max controller ports: {d}", .{controller.ports.len});

    // Program the Max Device Slots Enabled (MaxSlotsEn) field in the CONFIG register
    // Valid values are in the range of 0 to MaxSlots
    controller.ops.config = controller.slots.len;

    // Program the Device Context Base Address Array Pointer (DCBAAP) register
    const dcbaap_phys = phys.allocate(1, true) orelse {
        logger.err("Failed to allocate memory for DCBAAP", .{});
        return error.OutOfMemory;
    };
    const dcbaap = virt.asHigherHalfUncached([*]volatile u64, dcbaap_phys);

    // Populate the Device Context Base Address Array
    for (controller.slots.slice(), 0..) |*slot, i| {
        const input_context_phys = phys.allocate(1, true) orelse {
            logger.err("Failed to allocate memory for input context", .{});
            return error.OutOfMemory;
        };
        const device_context_phys = phys.allocate(1, true) orelse {
            logger.err("Failed to allocate memory for device context", .{});
            return error.OutOfMemory;
        };
        slot.input_context = virt.asHigherHalfUncached(*volatile anyopaque, input_context_phys);
        slot.device_context = virt.asHigherHalfUncached(*volatile anyopaque, device_context_phys);
        dcbaap[i + 1] = device_context_phys;

        const transfer_ring_phys = phys.allocate(1, true) orelse {
            logger.err("Failed to allocate memory for transfer ring", .{});
            return error.OutOfMemory;
        };
        slot.ep0_queue = .{
            .data = virt.asHigherHalfUncached([*]volatile TransferRequestBlock, transfer_ring_phys),
        };
    }

    controller.ops.dcbaap = dcbaap_phys;

    // Define the Command Ring Dequeue Pointer by programming the Command Ring Control Register
    const command_ring_phys = phys.allocate(1, true) orelse {
        logger.err("Failed to allocate memory for command ring", .{});
        return error.OutOfMemory;
    };
    controller.command_queue.data = virt.asHigherHalfUncached([*]volatile TransferRequestBlock, command_ring_phys);

    // Setting bit 0 (CRCR.RCS) to 1 indicates that the TRBs are not ready
    controller.ops.crcr = command_ring_phys | (1 << 0);

    const xhci_vector = interrupts.allocateVector();
    const msi = device.getMsi() orelse {
        logger.err("Failed to find the MSI", .{});
        return error.MsiNotSupported;
    };

    interrupts.registerHandlerWithContext(xhci_vector, interruptHandler, @intFromPtr(controller));
    msi.enable(0, xhci_vector);

    // Allocate and initialize the Event Ring Segments
    const event_ring_phys = phys.allocate(1, true) orelse {
        logger.err("Failed to allocate memory for event ring", .{});
        return error.OutOfMemory;
    };
    controller.event_queue.data = virt.asHigherHalfUncached([*]volatile TransferRequestBlock, event_ring_phys);

    // Allocate the Event Ring Segment Table
    const event_ring_segment_table_phys = phys.allocate(1, true) orelse {
        logger.err("Failed to allocate memory for event ring segment table", .{});
        return error.OutOfMemory;
    };
    const event_ring_segment_table = virt.asHigherHalfUncached(
        [*]volatile EventRingSegmentTableEntry,
        event_ring_segment_table_phys,
    );

    // Initialize ERST table entries to point to and to define the size (in TRBs)
    // of the respective Event Ring Segment.
    event_ring_segment_table[0].segment_base_address = event_ring_phys;
    event_ring_segment_table[0].segment_size = trb_count;

    // Program the Event Ring Segment Table Size (ERSTSZ) register
    // with the number of segments described by the Event Ring Segment Table.
    controller.runtime.ir[0].erstsz = 1;

    // Program the Interrupter Event Ring Dequeue Pointer (ERDP) register
    // with the starting address of the first segment described by the Event Ring Segment Table.
    controller.runtime.ir[0].erdp = event_ring_phys;

    // Program the Interrupter Event Ring Segment Table Base Address (ERSTBA) register
    // with a 64-bit address pointer to where the Event Ring Segment Table is located.
    controller.runtime.ir[0].erstba = event_ring_segment_table_phys;

    // @todo: Initializing the Interval field of the Interrupt Moderation register
    // with the target interrupt moderation rate.

    // Enable the Interrupter by writing a 1 to the Interrupt Enable (IE) field of
    // the Interrupter Management register
    controller.runtime.ir[0].iman |= (1 << 1);

    // Turn the host controller ON via setting the USBCMD.RS bit to 1.
    controller.ops.usbcmd |= (1 << 0);

    // Wait for the controller to start
    while (controller.ops.usbsts & (1 << 0) != 0) {
        scheduler.yield();
    }

    // Read in the port status and control registers for each port
    var ext_caps_pointer = controller.caps.hccparams1 >> 16;
    while (true) {
        const ext_capability = virt.asHigherHalfUncached([*]volatile u32, bar0.base + ext_caps_pointer * 4);

        const cap_id = ext_capability[0] & 0xff;
        const next_cap_pointer = (ext_capability[0] >> 8) & 0xff;

        switch (cap_id) {
            1 => logger.info("Got USB legacy support capability", .{}),
            2 => {
                const name: [4]u8 = @bitCast(ext_capability[1]);
                if (std.mem.eql(u8, &name, "USB ")) {
                    const port_offset = ext_capability[2] & 0xff - 1;
                    const port_count = (ext_capability[2] >> 8) & 0xff;
                    const minor_version: u8 = @truncate(ext_capability[0] >> 16);
                    const major_version: u8 = @truncate(ext_capability[0] >> 24);
                    logger.debug(
                        "Got supported protocol capability: {d} ports at offset {d} with version {d}.{d}",
                        .{ port_count, port_offset, major_version, minor_version },
                    );
                    for (controller.ports.slice()[port_offset..][0..port_count]) |*port| {
                        port.usb_version_major = major_version;
                        port.usb_version_minor = minor_version;
                    }
                }
            },
            else => {},
        }

        if (next_cap_pointer == 0) {
            break;
        }

        ext_caps_pointer += next_cap_pointer;
    }

    for (controller.ports.slice(), 0..) |_, i| {
        controller.checkPortStatus(@intCast(i));
    }

    // Enable system bus interrupt generation by writing a 1 to Interrupter Enable (INTE)
    // flag of the USBCMD register
    controller.ops.usbcmd |= (1 << 2);

    while (true) {
        // logger.debug("Controller thread is running", .{});
        scheduler.yield();
    }
}

const PciAddress = packed struct {
    bus: u8,
    slot: u8,
    function: u8,
    reserved: u8 = undefined,
};

fn handleDevice(device: pci.Device) !void {
    logger.info(
        "Handling device {X:0>4}:{X:0>4}",
        .{ device.vendor_id().read(), device.device_id().read() },
    );

    asm volatile ("sti");
    const param: u32 = @bitCast(PciAddress{ .bus = device.bus, .slot = device.slot, .function = device.function });
    _ = try scheduler.startKernelThread(controllerThread, param);
}
