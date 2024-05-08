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
            self.data[self.offset][2] = 0;
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
    connected: bool = false,
    usb_version_major: u8 = 0,
    usb_version_minor: u8 = 0,
    port_speed: u4 = 0,

    slot_id: u8 = 0,
    enable_slot_trb: ?*const TransferRequestBlock = null,
};

const SlotState = enum {
    unaddressed,
    addressed,
    fetch_configuration_descriptors,
    fetch_full_configuration_descriptors,
    identify,
    configured,
};

const Slot = struct {
    input_context: *volatile anyopaque,
    device_context: *volatile anyopaque,

    state: SlotState,
    devices: std.BoundedArray(Device, 8),

    descriptor_buffer: u64,
    descriptor_size: u16 = 0,

    ep_queue: [31]WriteQueue,
};

const Controller = struct {
    caps: *volatile CapabilityRegs,
    ops: *volatile OperationalRegs,
    runtime: *volatile RuntimeRegs,
    doorbells: [*]volatile u32,

    event_queue: ReadQueue,
    command_queue: WriteQueue,
    pending_irqs: scheduler.Semaphore = .{ .available = 0 },

    ports: std.BoundedArray(Port, 256) = .{},
    slots: std.BoundedArray(Slot, 256) = .{},

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

    fn getInputControlContext(self: *@This(), slot_id: usize) *volatile InputControlContext {
        return self.getInputContext(InputControlContext, slot_id, 0);
    }

    fn getInputSlotContext(self: *@This(), slot_id: usize) *volatile SlotContext {
        return self.getInputContext(SlotContext, slot_id, 1);
    }

    fn getInputEndpointContext(self: *@This(), slot_id: usize, endpoint_id: usize) *volatile EndpointContext {
        return self.getInputContext(EndpointContext, slot_id, 2 + endpoint_id);
    }

    fn getDeviceSlotContext(self: *@This(), slot_id: usize) *volatile SlotContext {
        return self.getDeviceContext(SlotContext, slot_id, 0);
    }

    fn getDeviceEndpointContext(self: *@This(), slot_id: usize, endpoint_id: usize) *volatile EndpointContext {
        return self.getDeviceContext(EndpointContext, slot_id, 1 + endpoint_id);
    }

    fn updateInputContext(self: *@This(), slot_id: usize) void {
        const slot = self.slots.get(slot_id);
        const stride = self.caps.contextStride();
        const input_context: [*]u8 = @ptrCast(@volatileCast(slot.input_context));
        const output_context: [*]u8 = @ptrCast(@volatileCast(slot.device_context));
        @memcpy(input_context[stride .. stride * 33], output_context[0 .. stride * 32]);
        @memset(input_context[0..8], 0);
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
        interrupt_on_completion: bool,
    ) void {
        std.debug.assert(endpoint == 0);
        const slot = &self.slots.slice()[slot_id];
        const has_data_stage = length > 0;
        const trt: u32 = if (has_data_stage) (request_type >> 7) | 0x2 else 0;
        _ = slot.ep_queue[0].enqueue(
            @as(u32, request_type) | @as(u32, request) << 8 | @as(u32, value) << 16,
            @as(u32, index) | @as(u32, length) << 16,
            8,
            (1 << 6) | (2 << 10) | (trt << 16),
        );
        if (has_data_stage) {
            _ = slot.ep_queue[0].enqueue(
                @truncate(buffer),
                @intCast(buffer >> 32),
                length,
                (@as(u32, @intFromBool(interrupt_on_completion)) << 5) |
                    (1 << 2) | (3 << 10) | @as(u32, request_type >> 7) << 16,
            );
        }
        const status_dir = (@as(u32, @intFromBool(interrupt_on_completion and !has_data_stage)) << 5) |
            (4 << 10) | @as(u32, (request_type >> 7) ^ 1 | @intFromBool(!has_data_stage)) << 16;
        _ = slot.ep_queue[0].enqueue(0, 0, 0, status_dir);
    }

    fn sendDataTransfer(
        self: *@This(),
        slot_id: u8,
        endpoint_addr: u8,
        length: u16,
        buffer: u64,
        interrupt_on_completion: bool,
    ) void {
        const slot = &self.slots.slice()[slot_id];
        const ep_queue = &slot.ep_queue[getEndpointIndex(endpoint_addr)];
        _ = ep_queue.enqueue(
            @truncate(buffer),
            @intCast(buffer >> 32),
            length,
            (1 << 2) | @as(u32, @intFromBool(interrupt_on_completion)) << 5 | (1 << 10),
        );
    }

    fn getEndpointIndex(endpoint_addr: u8) usize {
        const endpoint_num = (endpoint_addr & 0xf);
        if (endpoint_num == 0) {
            return 0;
        }
        return ((endpoint_num << 1) | (endpoint_addr >> 7)) - 1;
    }

    fn getEndpointAddr(endpoint_id: usize) u8 {
        if (endpoint_id == 0) {
            return 0;
        }
        const temp: u8 = @intCast(endpoint_id + 1);
        return (temp >> 1) | (temp << 7);
    }

    fn enableEndpoint(self: *@This(), slot_id: usize, endpoint: Endpoint) void {
        const endpoint_id = getEndpointIndex(endpoint.address);
        const slot = &self.slots.slice()[slot_id];
        const ep_queue = &slot.ep_queue[endpoint_id];

        const input_control_context = self.getInputControlContext(slot_id);
        input_control_context.a |= @as(u32, 1) << @intCast(endpoint_id + 1);

        const ep_type: u2 = @truncate(endpoint.attributes);
        const ep_context = self.getInputEndpointContext(slot_id, endpoint_id);
        ep_context.dw1.ep_type = ep_type;
        ep_context.dw1.ep_type |= @intCast((endpoint.address & 0x80) >> 5);

        switch (ep_type) {
            0b00 => { // control
                ep_context.dw1.ep_type = 4;
            },
            0b10 => {}, // bulk
            0b01, 0b11 => { // interrupt
                logger.debug("Enabling interrupt endpoint {d} for slot {d}", .{ endpoint_id, slot_id });
                ep_context.dw1.max_packet_size = endpoint.packet_size & 0x7FF;
                ep_context.dw1.max_burst_size = @truncate((endpoint.packet_size & 0x1800) >> 11);
                ep_context.dw0.mult = 0;
                ep_context.dw1.error_count = if (ep_type == 0b11) 3 else 0;
                ep_context.tr_dequeue_ptr = virt.higherHalfUncachedToPhysical(ep_queue.data) | ep_queue.cycle_state;
                ep_context.dw4.max_esit_payload = ep_context.dw1.max_packet_size;
                ep_context.dw0.interval = 6; // @todo: not hardcode 8ms
            },
        }
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
        port.port_speed = @truncate(portsc_value >> 10);

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

const Endpoint = extern struct {
    length: u8,
    descriptor_type: u8,
    address: u8,
    attributes: u8,
    packet_size: u16 align(1),
    interval: u8,
};

const Device = struct {
    const HidDevice = struct {
        poll_ep: Endpoint = undefined,
        report_buffer: u64 = 0,

        fn configure(self: *@This(), device: *Device, controller: *Controller, slot_id: usize) void {
            controller.sendControlTransfer(@intCast(slot_id), 0, 0x21, 0xB, 0, device.interface_id, 0, 0, false);
            controller.sendControlTransfer(@intCast(slot_id), 0, 0x21, 0xA, 0, device.interface_id, 0, 0, false);
            controller.enableEndpoint(slot_id, self.poll_ep);
            self.report_buffer = phys.allocate(1, true).?;
        }

        fn onConfigured(self: *@This(), device: *Device, controller: *Controller, slot_id: usize) void {
            _ = device;
            controller.sendDataTransfer(@intCast(slot_id), self.poll_ep.address, 8, self.report_buffer, true);
        }

        fn onDataTransferComplete(
            self: *@This(),
            device: *Device,
            controller: *Controller,
            endpoint_addr: u8,
            slot_id: usize,
            data: []const u8,
        ) bool {
            _ = device;
            if (endpoint_addr == self.poll_ep.address) {
                logger.debug("HID data transfer complete: {any}", .{std.fmt.fmtSliceHexUpper(data)});
                controller.sendDataTransfer(@intCast(slot_id), self.poll_ep.address, 8, self.report_buffer, true);
                return true;
            }
            return false;
        }
    };

    interface_id: u8 = undefined,
    impl: union(enum) {
        hid_keyboard: HidDevice,
        hid_mouse: HidDevice,
        mass_storage: struct {
            protocol: u8,
        },
    },

    fn configure(self: *@This(), controller: *Controller, slot_id: usize) void {
        switch (self.impl) {
            .hid_keyboard, .hid_mouse => |*hid| hid.configure(self, controller, slot_id),
            .mass_storage => logger.err("Mass storage device configuration not implemented", .{}),
        }
    }

    fn onConfigured(self: *@This(), controller: *Controller, slot_id: usize) void {
        switch (self.impl) {
            .hid_keyboard, .hid_mouse => |*hid| hid.onConfigured(self, controller, slot_id),
            .mass_storage => {},
        }
    }

    fn onDataTransferComplete(
        self: *@This(),
        controller: *Controller,
        endpoint_addr: u8,
        slot_id: usize,
        data: []const u8,
    ) bool {
        switch (self.impl) {
            .hid_keyboard, .hid_mouse => |*hid| return hid.onDataTransferComplete(self, controller, endpoint_addr, slot_id, data),
            .mass_storage => {},
        }

        return false;
    }

    fn handleDescriptor(self: *@This(), descriptor: []const u8) void {
        const descriptor_type = descriptor[1];
        logger.debug(
            "{s}: Handling descriptor of type {d} {any}",
            .{ @tagName(self.impl), descriptor_type, std.fmt.fmtSliceHexUpper(descriptor) },
        );
        switch (descriptor_type) {
            5 => {
                const endpoint = std.mem.bytesToValue(Endpoint, descriptor[0..7]);
                switch (self.impl) {
                    .hid_keyboard, .hid_mouse => |*hid| {
                        if ((endpoint.attributes & 0b11) == 0b11 and (endpoint.address & 0x80) != 0) {
                            logger.debug("Found HID polling endpoint {any} for {s}", .{ endpoint, @tagName(self.impl) });
                            hid.poll_ep = endpoint;
                        }
                    },
                    .mass_storage => {},
                }
            },
            else => {},
        }
    }
};

fn identifyDevice(config_descriptor: []const u8) !std.BoundedArray(Device, 8) {
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
                } else if (interface_class == 0x8 and interface_subclass == 0x50) {
                    current_device = try devices.addOne();
                    current_device.?.* = .{ .impl = .{
                        .mass_storage = .{ .protocol = interface_protocol },
                    } };
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

fn interruptHandler(context: u64) void {
    const controller: *Controller = @ptrFromInt(context);
    controller.pending_irqs.release(1);
    apic.eoi();
}

fn processIrqs(controller: *Controller) void {
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

                const completed_trb_id: u6 = @truncate(completed_trb[3] >> 10);
                const slot_id = @as(u8, @truncate(trb[3] >> 24)) - 1;
                const endpoint_id: u5 = @truncate(trb[3] >> 16);
                const slot = &controller.slots.slice()[slot_id];
                const descriptor = virt.asHigherHalf([*]u8, slot.descriptor_buffer)[0..slot.descriptor_size];
                switch (slot.state) {
                    .addressed => {
                        slot.descriptor_size = descriptor[0];

                        const usb_major = descriptor[3];
                        const max_packet_size: u16 = if (usb_major < 3)
                            descriptor[7]
                        else
                            @as(u16, 1) << @intCast(descriptor[7]);

                        const input_control_context = controller.getInputControlContext(slot_id);
                        input_control_context.a = (1 << 1);

                        const input_ep0_context = controller.getInputEndpointContext(slot_id, 0);
                        input_ep0_context.dw1.max_packet_size = max_packet_size;

                        const input_context_phys = virt.higherHalfUncachedToPhysical(slot.input_context);
                        _ = controller.command_queue.enqueue(
                            @truncate(input_context_phys),
                            @intCast(input_context_phys >> 32),
                            0,
                            @as(u32, slot_id + 1) << 24 | (13 << 10),
                        );
                    },
                    .fetch_configuration_descriptors => {
                        const num_configurations = descriptor[17];
                        const first_config_buffer = slot.descriptor_buffer + slot.descriptor_size;
                        logger.debug("Device has {d} configuration descriptors", .{num_configurations});

                        for (0..num_configurations) |i| {
                            _ = controller.sendControlTransfer(
                                slot_id,
                                0,
                                0x80,
                                6,
                                (2 << 8) | @as(u16, @intCast(i)),
                                0,
                                4,
                                first_config_buffer + i * 4,
                                i == num_configurations - 1,
                            );
                        }

                        slot.state = .fetch_full_configuration_descriptors;
                    },
                    .fetch_full_configuration_descriptors => {
                        const num_configurations = descriptor[17];

                        var offset: usize = slot.descriptor_size;
                        for (0..num_configurations) |i| {
                            const config_descriptor = virt.asHigherHalf(
                                [*]u8,
                                slot.descriptor_buffer + slot.descriptor_size + i * 4,
                            )[0..4];
                            const total_length = std.mem.readInt(u16, config_descriptor[2..4], .little);
                            logger.debug("Fetching configuration descriptor {d} (size {d})", .{ i, total_length });
                            std.debug.assert(offset + total_length <= std.mem.page_size);
                            _ = controller.sendControlTransfer(
                                slot_id,
                                0,
                                0x80,
                                6,
                                (2 << 8) | @as(u16, @intCast(i)),
                                0,
                                total_length,
                                slot.descriptor_buffer + offset,
                                i == num_configurations - 1,
                            );
                            offset += total_length;
                        }

                        slot.state = .identify;
                    },
                    .identify => {
                        const num_configurations = descriptor[17];
                        logger.debug("Device descriptor for slot {d}: {any}", .{
                            slot_id,
                            std.fmt.fmtSliceHexUpper(descriptor),
                        });
                        var offset: usize = slot.descriptor_size;
                        var config_value: ?u8 = null;
                        var best_devices: ?std.BoundedArray(Device, 8) = null;
                        for (0..num_configurations) |_| {
                            const config_descriptor = virt.asHigherHalf([*]u8, slot.descriptor_buffer + offset);
                            const total_length = std.mem.readInt(u16, config_descriptor[2..4], .little);
                            offset += total_length;
                            const devices = identifyDevice(config_descriptor[0..total_length]) catch continue;
                            if (best_devices == null or devices.len > best_devices.?.len) {
                                best_devices = devices;
                                config_value = config_descriptor[5];
                            }
                        }

                        if (best_devices) |devices| {
                            slot.devices = devices;
                            slot.state = .configured;

                            const input_slot_context = controller.getInputSlotContext(slot_id);
                            input_slot_context.dw0.context_entries = 31;

                            const input_control_context = controller.getInputControlContext(slot_id);
                            input_control_context.a |= (1 << 0);
                            input_control_context.dw7.configuration_value = config_value.?;

                            const input_context_phys = virt.higherHalfUncachedToPhysical(slot.input_context);
                            _ = controller.sendControlTransfer(slot_id, 0, 0, 9, config_value.?, 0, 0, 0, true);

                            for (slot.devices.slice()) |*device| {
                                logger.info("Identified device {s} on interface {d}", .{ @tagName(device.impl), device.interface_id });
                                device.configure(controller, slot_id);
                            }

                            _ = controller.command_queue.enqueue(
                                @truncate(input_context_phys),
                                @intCast(input_context_phys >> 32),
                                0,
                                @as(u32, slot_id + 1) << 24 | (12 << 10),
                            );
                        }
                    },
                    .configured => {
                        var data: []const u8 = &.{};
                        switch (completed_trb_id) {
                            1, 2 => { // Data stage or normal TRB
                                const buffer = virt.asHigherHalf([*]u8, @as(u64, completed_trb[0]) | @as(u64, completed_trb[1]) << 32);
                                const buffer_length: u16 = @truncate(completed_trb[2]);
                                const length_remaining: u24 = @truncate(trb[2]);
                                data = buffer[0 .. buffer_length - length_remaining];
                            },
                            4 => {}, // Status stage, control TRB without data
                            else => unreachable,
                        }

                        const endpoint_addr = Controller.getEndpointAddr(endpoint_id - 1);
                        for (slot.devices.slice()) |*device| {
                            if (device.onDataTransferComplete(controller, endpoint_addr, slot_id, data)) {
                                break;
                            }
                        }
                    },
                    .unaddressed => unreachable,
                }
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
                            const input_control_context = controller.getInputControlContext(slot_id);
                            input_control_context.a = (1 << 0) | (1 << 1);

                            const input_slot_context = controller.getInputSlotContext(slot_id);
                            input_slot_context.dw1.root_hub_port_number = port_num + 1;
                            input_slot_context.dw0.route_string = 0;
                            input_slot_context.dw0.speed = related_port.port_speed;
                            input_slot_context.dw0.context_entries = 1;

                            const input_ep0_context = controller.getInputEndpointContext(slot_id, 0);
                            input_ep0_context.dw1.ep_type = 4; // Control Endpoint
                            input_ep0_context.dw1.max_packet_size = switch (related_port.port_speed) {
                                2 => 8, // Low Speed,
                                1, 3 => 64, // Full Speed/High Speed,
                                4, 5 => 512, // Super Speed/ Super Speed Plus,
                                else => unreachable,
                            };
                            input_ep0_context.dw1.max_burst_size = 0;
                            input_ep0_context.tr_dequeue_ptr = virt.higherHalfUncachedToPhysical(slot.ep_queue[0].data) | slot.ep_queue[0].cycle_state;
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
                        const slot_id = @as(u8, @truncate(trb[3] >> 24)) - 1;
                        const slot = &controller.slots.slice()[slot_id];
                        std.debug.assert(slot.state == .unaddressed);
                        slot.descriptor_size = 8;
                        slot.state = .addressed;
                        controller.updateInputContext(slot_id);
                        controller.sendControlTransfer(slot_id, 0, 0x80, 6, (1 << 8), 0, slot.descriptor_size, slot.descriptor_buffer, true);
                    },
                    // Configure Endpoint Command
                    12 => {
                        const slot_id = @as(u8, @truncate(trb[3] >> 24)) - 1;
                        const slot = &controller.slots.slice()[slot_id];
                        std.debug.assert(slot.state == .configured);
                        controller.updateInputContext(slot_id);
                        for (slot.devices.slice()) |*device| {
                            device.onConfigured(controller, slot_id);
                        }
                    },
                    // Evaluate Context Command
                    13 => {
                        const slot_id = @as(u8, @truncate(trb[3] >> 24)) - 1;
                        const slot = &controller.slots.slice()[slot_id];
                        std.debug.assert(slot.state == .addressed);
                        slot.state = .fetch_configuration_descriptors;
                        controller.updateInputContext(slot_id);
                        controller.sendControlTransfer(slot_id, 0, 0x80, 6, (1 << 8), 0, slot.descriptor_size, slot.descriptor_buffer, true);
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
                logger.debug("Unhandled TRB {X} {X} {X} {X} {X}", .{ trb_id, trb[0], trb[1], trb[2], trb[3] });
            },
        }
    }

    // Signal to the controller the last TRB that was processed
    const next_trb = &controller.event_queue.data[controller.event_queue.offset];
    controller.runtime.ir[0].erdp = virt.higherHalfUncachedToPhysical(next_trb) | (1 << 3);

    // Clear the IMAN.IP flag
    controller.runtime.ir[0].iman = (1 << 0) | (1 << 1);

    if (controller.command_queue.dirty) {
        controller.command_queue.dirty = false;
        controller.doorbells[0] = 0;
    }

    for (controller.slots.slice(), 0..) |*slot, i| {
        for (&slot.ep_queue, 1..) |*ep_queue, j| {
            if (ep_queue.dirty) {
                ep_queue.dirty = false;
                controller.doorbells[i + 1] = @intCast(j);
            }
        }
    }
}

fn controllerThread(param: u32) !void {
    const address: PciAddress = @bitCast(param);
    const device = pci.Device{ .bus = address.bus, .slot = address.slot, .function = address.function };

    const bar0 = device.getBar(0).?;
    const caps = virt.asHigherHalfUncached(*volatile CapabilityRegs, bar0.base);

    const controller = try root.allocator.create(Controller);
    controller.* = .{
        .caps = caps,
        .ops = virt.asHigherHalfUncached(*volatile OperationalRegs, bar0.base + caps.caplength),
        .runtime = virt.asHigherHalfUncached(*volatile RuntimeRegs, bar0.base + caps.rtsoff),
        .doorbells = virt.asHigherHalfUncached([*]volatile u32, bar0.base + caps.dboff),

        .command_queue = .{ .data = undefined },
        .event_queue = .{ .data = undefined },
    };

    try controller.ports.resize(@as(u8, @truncate(caps.hcsparams1 >> 24)));
    try controller.slots.resize(@as(u8, @truncate(caps.hcsparams1)));

    for (controller.ports.slice()) |*port| {
        port.* = .{};
    }

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
        dcbaap[i + 1] = device_context_phys;
        slot.* = .{
            .input_context = virt.asHigherHalfUncached(*volatile anyopaque, input_context_phys),
            .device_context = virt.asHigherHalfUncached(*volatile anyopaque, device_context_phys),
            .state = .unaddressed,
            .devices = .{},
            .descriptor_buffer = phys.allocate(1, false) orelse {
                logger.err("Failed to allocate memory for descriptor buffer", .{});
                return error.OutOfMemory;
            },
            .ep_queue = undefined,
        };
        for (&slot.ep_queue) |*ep_queue| {
            const transfer_ring_phys = phys.allocate(1, true) orelse {
                logger.err("Failed to allocate memory for transfer ring", .{});
                return error.OutOfMemory;
            };
            ep_queue.* = .{ .data = virt.asHigherHalfUncached([*]volatile TransferRequestBlock, transfer_ring_phys) };
        }
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

    // Set up scratchpad buffers
    const scratchpads_hi: u5 = @truncate(controller.caps.hcsparams2 >> 21);
    const scratchpads_lo: u5 = @truncate(controller.caps.hcsparams2 >> 27);
    const scratchpad_count = scratchpads_lo | @as(u10, scratchpads_hi) << 5;
    if (scratchpad_count > 0) {
        std.debug.assert(scratchpad_count <= 512);
        logger.debug("Controller requested {d} scratchpad pages", .{scratchpad_count});

        const scratchpad_array_phys = phys.allocate(1, true) orelse {
            logger.err("Failed to allocate memory for scratchpad array", .{});
            return error.OutOfMemory;
        };
        dcbaap[0] = scratchpad_array_phys;

        const scratchpad_array = virt.asHigherHalf([*]u64, scratchpad_array_phys);
        for (0..scratchpad_count) |i| {
            scratchpad_array[i] = phys.allocate(1, true) orelse {
                logger.err("Failed to allocate memory for scratchpad buffer", .{});
                return error.OutOfMemory;
            };
        }
    }

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
        controller.pending_irqs.acquire(1);
        processIrqs(controller);
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
