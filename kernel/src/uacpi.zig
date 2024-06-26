const logger = std.log.scoped(.uacpi);

const C = @cImport({
    @cInclude("uacpi/acpi.h");
    @cInclude("uacpi/event.h");
    @cInclude("uacpi/notify.h");
    @cInclude("uacpi/resources.h");
    @cInclude("uacpi/sleep.h");
    @cInclude("uacpi/tables.h");
    @cInclude("uacpi/uacpi.h");
    @cInclude("uacpi/utilities.h");
});

const limine = @import("limine");
const root = @import("root");
const std = @import("std");

const apic = @import("./apic.zig");
const arch = @import("./arch.zig");
const interrupts = @import("./interrupts.zig");
const lock = @import("./lock.zig");
const pci = @import("./pci.zig");
const hpet = @import("./hpet.zig");
const virt = @import("./virt.zig");

pub usingnamespace C;

pub const x_uacpi_namespace_node_info = extern struct {
    size: C.uacpi_u32,
    name: C.uacpi_object_name,
    type_: C.uacpi_object_type,
    num_params: C.uacpi_u8,
    has_flags: C.uacpi_u8,
    sxd: [4]C.uacpi_u8,
    sxw: [5]C.uacpi_u8,
    adr: C.uacpi_u64,
    hid: C.uacpi_id_string,
    uid: C.uacpi_id_string,
    cls: C.uacpi_id_string,
    cid: C.uacpi_pnp_id_list,
};

fn powerButtonHandler(_: C.uacpi_handle) callconv(.C) C.uacpi_interrupt_ret {
    logger.debug("Shutting down the system", .{});

    var status = C.uacpi_prepare_for_sleep_state(C.UACPI_SLEEP_STATE_S5);
    if (status != C.UACPI_STATUS_OK) {
        logger.err("Failed to prepare for S5 sleep: {s}", .{C.uacpi_status_to_string(status)});
        return C.UACPI_INTERRUPT_HANDLED;
    }

    logger.debug("Entering S5 sleep", .{});

    status = C.uacpi_enter_sleep_state(C.UACPI_SLEEP_STATE_S5);
    if (status != C.UACPI_STATUS_OK) {
        logger.err("Failed to enter S5 sleep: {s}", .{C.uacpi_status_to_string(status)});
        return C.UACPI_INTERRUPT_HANDLED;
    }

    unreachable;
}

pub fn initTables(rsdp_res: *limine.RsdpResponse) !void {
    var init_params: C.uacpi_init_params = .{
        .rsdp = virt.higherHalfToPhysical(rsdp_res.address),
        .no_acpi_mode = false,
        .rt_params = .{
            .log_level = C.UACPI_LOG_INFO,
            .flags = 0,
        },
    };

    const status = C.uacpi_initialize(&init_params);
    if (status != C.UACPI_STATUS_OK) {
        logger.err("Failed to initialize uACPI: {s}", .{C.uacpi_status_to_string(status)});
        return error.InitializationFailed;
    }
}

pub fn init() !void {
    var status = C.uacpi_namespace_load();
    if (status != C.UACPI_STATUS_OK) {
        logger.err("Failed to load namespace: {s}", .{C.uacpi_status_to_string(status)});
        return error.InitializationFailed;
    }

    status = C.uacpi_namespace_initialize();
    if (status != C.UACPI_STATUS_OK) {
        logger.err("Failed to initialize namespace: {s}", .{C.uacpi_status_to_string(status)});
        return error.InitializationFailed;
    }

    status = C.uacpi_finalize_gpe_initialization();
    if (status != C.UACPI_STATUS_OK) {
        logger.err("Failed to finalize GPE initialization: {s}", .{C.uacpi_status_to_string(status)});
        return error.InitializationFailed;
    }

    status = C.uacpi_set_interrupt_model(C.UACPI_INTERRUPT_MODEL_IOAPIC);
    if (status != C.UACPI_STATUS_OK) {
        logger.err("Failed to set interrupt model: {s}", .{C.uacpi_status_to_string(status)});
        return error.InitializationFailed;
    }

    status = C.uacpi_install_fixed_event_handler(C.UACPI_FIXED_EVENT_POWER_BUTTON, powerButtonHandler, null);
    if (status != C.UACPI_STATUS_OK) {
        logger.err("Failed to install power button handler: {s}", .{C.uacpi_status_to_string(status)});
        return error.InitializationFailed;
    }
}

export fn uacpi_kernel_raw_memory_read(
    address: C.uacpi_phys_addr,
    byte_width: C.uacpi_u8,
    out_value: *C.uacpi_u64,
) callconv(.C) C.uacpi_status {
    switch (byte_width) {
        1 => out_value.* = virt.asHigherHalfUncached(*const volatile u8, address).*,
        2 => out_value.* = virt.asHigherHalfUncached(*const volatile u16, address).*,
        4 => out_value.* = virt.asHigherHalfUncached(*const volatile u32, address).*,
        8 => out_value.* = virt.asHigherHalfUncached(*const volatile u64, address).*,
        else => return C.UACPI_STATUS_INVALID_ARGUMENT,
    }
    return C.UACPI_STATUS_OK;
}

export fn uacpi_kernel_raw_memory_write(
    address: C.uacpi_phys_addr,
    byte_width: C.uacpi_u8,
    in_value: C.uacpi_u64,
) callconv(.C) C.uacpi_status {
    switch (byte_width) {
        1 => virt.asHigherHalfUncached(*volatile u8, address).* = @truncate(in_value),
        2 => virt.asHigherHalfUncached(*volatile u16, address).* = @truncate(in_value),
        4 => virt.asHigherHalfUncached(*volatile u32, address).* = @truncate(in_value),
        8 => virt.asHigherHalfUncached(*volatile u64, address).* = in_value,
        else => return C.UACPI_STATUS_INVALID_ARGUMENT,
    }
    return C.UACPI_STATUS_OK;
}

export fn uacpi_kernel_raw_io_read(
    address: C.uacpi_io_addr,
    byte_width: C.uacpi_u8,
    out_value: *C.uacpi_u64,
) callconv(.C) C.uacpi_status {
    const port: u16 = @intCast(address);
    switch (byte_width) {
        1 => out_value.* = arch.in(u8, port),
        2 => out_value.* = arch.in(u16, port),
        4 => out_value.* = arch.in(u32, port),
        else => return C.UACPI_STATUS_INVALID_ARGUMENT,
    }
    return C.UACPI_STATUS_OK;
}

export fn uacpi_kernel_raw_io_write(
    address: C.uacpi_io_addr,
    byte_width: C.uacpi_u8,
    in_value: C.uacpi_u64,
) callconv(.C) C.uacpi_status {
    const port: u16 = @intCast(address);
    switch (byte_width) {
        1 => arch.out(u8, port, @truncate(in_value)),
        2 => arch.out(u16, port, @truncate(in_value)),
        4 => arch.out(u32, port, @truncate(in_value)),
        else => return C.UACPI_STATUS_INVALID_ARGUMENT,
    }
    return C.UACPI_STATUS_OK;
}

export fn uacpi_kernel_pci_read(
    address: *C.uacpi_pci_address,
    offset: C.uacpi_size,
    byte_width: C.uacpi_u8,
    out_value: *C.uacpi_u64,
) callconv(.C) C.uacpi_status {
    const offset_: u32 = @intCast(offset);
    const pci_address: pci.Address = .{
        .segment = address.segment,
        .bus = address.bus,
        .device = @intCast(address.device),
        .function = @intCast(address.function),
    };
    switch (byte_width) {
        1 => out_value.* = pci.readConfigSpace(u8, pci_address, offset_),
        2 => out_value.* = pci.readConfigSpace(u16, pci_address, offset_),
        4 => out_value.* = pci.readConfigSpace(u32, pci_address, offset_),
        else => return C.UACPI_STATUS_INVALID_ARGUMENT,
    }
    return C.UACPI_STATUS_OK;
}

export fn uacpi_kernel_pci_write(
    address: *C.uacpi_pci_address,
    offset: C.uacpi_size,
    byte_width: C.uacpi_u8,
    in_value: C.uacpi_u64,
) callconv(.C) C.uacpi_status {
    const offset_: u32 = @intCast(offset);
    const pci_address: pci.Address = .{
        .segment = address.segment,
        .bus = address.bus,
        .device = @intCast(address.device),
        .function = @intCast(address.function),
    };
    switch (byte_width) {
        1 => pci.writeConfigSpace(u8, pci_address, offset_, @intCast(in_value)),
        2 => pci.writeConfigSpace(u16, pci_address, offset_, @intCast(in_value)),
        4 => pci.writeConfigSpace(u32, pci_address, offset_, @intCast(in_value)),
        else => return C.UACPI_STATUS_INVALID_ARGUMENT,
    }
    return C.UACPI_STATUS_OK;
}

export fn uacpi_kernel_io_map(
    base: C.uacpi_io_addr,
    _: C.uacpi_size,
    out_handle: *C.uacpi_handle,
) callconv(.C) C.uacpi_status {
    out_handle.* = @ptrFromInt(base);
    return C.UACPI_STATUS_OK;
}

export fn uacpi_kernel_io_unmap(_: C.uacpi_handle) callconv(.C) void {}

export fn uacpi_kernel_io_read(
    handle: C.uacpi_handle,
    offset: C.uacpi_size,
    byte_width: C.uacpi_u8,
    out_value: *C.uacpi_u64,
) callconv(.C) C.uacpi_status {
    const base: u64 = @intFromPtr(handle);
    return uacpi_kernel_raw_io_read(base + offset, byte_width, out_value);
}

export fn uacpi_kernel_io_write(
    handle: C.uacpi_handle,
    offset: C.uacpi_size,
    byte_width: C.uacpi_u8,
    in_value: C.uacpi_u64,
) callconv(.C) C.uacpi_status {
    const base: u64 = @intFromPtr(handle);
    return uacpi_kernel_raw_io_write(base + offset, byte_width, in_value);
}

export fn uacpi_kernel_map(addr: C.uacpi_phys_addr, _: C.uacpi_size) callconv(.C) *anyopaque {
    return virt.asHigherHalf(*anyopaque, addr);
}

export fn uacpi_kernel_unmap(_: *anyopaque, _: C.uacpi_size) callconv(.C) void {}

export fn uacpi_kernel_alloc(size: C.uacpi_size) callconv(.C) *anyopaque {
    const result = root.allocator.rawAlloc(@max(size, 8), 3, @returnAddress());
    if (result == null) {
        @panic("uacpi_kernel_alloc failed");
    }
    return @ptrCast(result);
}

export fn uacpi_kernel_calloc(count: C.uacpi_size, size: C.uacpi_size) callconv(.C) *anyopaque {
    const result: [*]u8 = @ptrCast(uacpi_kernel_alloc(count * size));
    @memset(result[0 .. count * size], 0);
    return result;
}

export fn uacpi_kernel_free(mem: ?*anyopaque, size: C.uacpi_size) callconv(.C) void {
    if (mem != null) {
        const result: [*]u8 = @ptrCast(mem.?);
        root.allocator.rawFree(result[0..@max(size, 8)], 3, @returnAddress());
    }
}

export fn uacpi_kernel_log(level: C.uacpi_log_level, message: [*c]const C.uacpi_u8) callconv(.C) void {
    const length = std.mem.len(message);
    const msg = message[0 .. length - 1];
    switch (level) {
        C.UACPI_LOG_DEBUG, C.UACPI_LOG_TRACE => logger.debug("{s}", .{msg}),
        C.UACPI_LOG_INFO => logger.info("{s}", .{msg}),
        C.UACPI_LOG_WARN => logger.warn("{s}", .{msg}),
        C.UACPI_LOG_ERROR => logger.err("{s}", .{msg}),
        else => unreachable,
    }
}

export fn uacpi_kernel_get_ticks() callconv(.C) C.uacpi_u64 {
    logger.warn("uacpi_kernel_get_ticks is a stub", .{});
    return 0;
}

export fn uacpi_kernel_stall(usec: C.uacpi_u8) callconv(.C) void {
    hpet.sleep(@as(usize, usec) * std.time.ns_per_us, false);
}

export fn uacpi_kernel_sleep(msec: C.uacpi_u64) callconv(.C) void {
    hpet.sleep(msec * std.time.ns_per_ms, true);
}

export fn uacpi_kernel_create_mutex() callconv(.C) C.uacpi_handle {
    const result = root.allocator.create(lock.Spinlock) catch {
        @panic("uacpi_kernel_create_mutex failed");
    };
    result.* = .{};
    return @ptrCast(result);
}

export fn uacpi_kernel_free_mutex(handle: C.uacpi_handle) callconv(.C) void {
    if (handle) |ptr| {
        const mtx: *lock.Spinlock = @ptrCast(@alignCast(ptr));
        root.allocator.destroy(mtx);
    }
}

export fn uacpi_kernel_acquire_mutex(
    handle: C.uacpi_handle,
    timeout: C.uacpi_u16,
) callconv(.C) C.uacpi_bool {
    if (timeout != 0xFFFF) {
        logger.warn("uacpi_kernel_acquire_mutex does not support timeout", .{});
    }

    const mtx: *lock.Spinlock = @ptrCast(@alignCast(handle));
    mtx.lock();
    return true;
}

export fn uacpi_kernel_release_mutex(handle: C.uacpi_handle) callconv(.C) void {
    const mtx: *lock.Spinlock = @ptrCast(@alignCast(handle));
    mtx.unlock();
}

export fn uacpi_kernel_create_event() callconv(.C) C.uacpi_handle {
    logger.warn("uacpi_kernel_create_event is a stub", .{});
    return @ptrFromInt(0x1);
}

export fn uacpi_kernel_free_event(event: C.uacpi_handle) callconv(.C) void {
    _ = event;
    logger.warn("uacpi_kernel_free_event is a stub", .{});
    unreachable;
}

export fn uacpi_kernel_wait_for_event(
    event: C.uacpi_handle,
    timeout: C.uacpi_u16,
) callconv(.C) C.uacpi_bool {
    _ = event;
    _ = timeout;
    logger.warn("uacpi_kernel_wait_for_event is a stub", .{});
    unreachable;
}

export fn uacpi_kernel_signal_event(event: C.uacpi_handle) callconv(.C) void {
    _ = event;
    logger.warn("uacpi_kernel_signal_event is a stub", .{});
    unreachable;
}

export fn uacpi_kernel_reset_event(event: C.uacpi_handle) callconv(.C) void {
    _ = event;
    logger.warn("uacpi_kernel_reset_event is a stub", .{});
    unreachable;
}

export fn uacpi_kernel_handle_firmware_request(
    request: *C.uacpi_firmware_request,
) callconv(.C) C.uacpi_status {
    switch (request.type) {
        C.UACPI_FIRMWARE_REQUEST_TYPE_BREAKPOINT => {
            logger.warn("uacpi_kernel_handle_firmware_request with type BREAKPOINT is a stub", .{});
        },
        C.UACPI_FIRMWARE_REQUEST_TYPE_FATAL => {
            logger.err("uacpi_kernel_handle_firmware_request: fatal", .{});
            logger.err("fatal request type: {x}", .{request.unnamed_0.fatal.type});
            logger.err("fatal request code: {x}", .{request.unnamed_0.fatal.code});
            logger.err("fatal request arg: {x}", .{request.unnamed_0.fatal.arg});
        },
        else => {
            logger.warn("uacpi_kernel_handle_firmware_request: unknown type {d}", .{request.type});
            return C.UACPI_STATUS_INVALID_ARGUMENT;
        },
    }

    return C.UACPI_STATUS_OK;
}

const UACPIIRQContext = struct {
    handler: C.uacpi_interrupt_handler,
    uacpi_ctx: C.uacpi_handle,
};

fn uacpiInterruptHandler(context: ?*anyopaque) void {
    const ctx: *UACPIIRQContext = @ptrCast(@alignCast(context.?));
    _ = ctx.handler.?(ctx.uacpi_ctx);
}

export fn uacpi_kernel_install_interrupt_handler(
    irq: C.uacpi_u32,
    handler: C.uacpi_interrupt_handler,
    ctx: C.uacpi_handle,
    out_irq_handle: *C.uacpi_handle,
) callconv(.C) C.uacpi_status {
    const irq_handle = root.allocator.create(UACPIIRQContext) catch {
        @panic("uacpi_kernel_install_interrupt_handler failed");
    };
    irq_handle.* = .{ .handler = handler, .uacpi_ctx = ctx };
    const vector = interrupts.allocateVector();
    interrupts.registerHandlerWithContext(vector, uacpiInterruptHandler, irq_handle);
    _ = apic.routeISAIRQ(@intCast(irq), 0, vector, false);
    out_irq_handle.* = @ptrCast(irq_handle);
    return C.UACPI_STATUS_OK;
}

export fn uacpi_kernel_uninstall_interrupt_handler(
    handler: C.uacpi_interrupt_handler,
    irq_handle: C.uacpi_handle,
) callconv(.C) C.uacpi_status {
    _ = handler;
    _ = irq_handle;
    logger.warn("uacpi_kernel_uninstall_interrupt_handler is a stub", .{});
    return C.UACPI_STATUS_OK;
}

export fn uacpi_kernel_create_spinlock() callconv(.C) C.uacpi_handle {
    logger.warn("uacpi_kernel_create_spinlock is a stub", .{});
    return uacpi_kernel_create_mutex();
}

export fn uacpi_kernel_free_spinlock(spinlock: C.uacpi_handle) callconv(.C) void {
    uacpi_kernel_free_mutex(spinlock);
}

export fn uacpi_kernel_spinlock_lock(spinlock: C.uacpi_handle) callconv(.C) C.uacpi_cpu_flags {
    _ = uacpi_kernel_acquire_mutex(spinlock, 0xFFFF);
    return 0;
}

export fn uacpi_kernel_spinlock_unlock(spinlock: C.uacpi_handle, _: C.uacpi_cpu_flags) callconv(.C) void {
    uacpi_kernel_release_mutex(spinlock);
}

export fn uacpi_kernel_schedule_work(
    work_type: C.uacpi_work_type,
    handler: C.uacpi_work_handler,
    ctx: C.uacpi_handle,
) callconv(.C) C.uacpi_status {
    _ = work_type;
    logger.warn("uacpi_kernel_schedule_work is a stub", .{});
    handler.?(ctx);
    return C.UACPI_STATUS_OK;
}

export fn uacpi_kernel_wait_for_work_completion() callconv(.C) C.uacpi_status {
    logger.warn("uacpi_kernel_wait_for_work_completion is a stub", .{});
    return C.UACPI_STATUS_OK;
}
