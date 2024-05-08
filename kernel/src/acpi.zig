const logger = std.log.scoped(.acpi);

pub const C = @cImport({
    @cInclude("uacpi/uacpi.h");
    @cInclude("uacpi/acpi.h");
    @cInclude("uacpi/notify.h");
    @cInclude("uacpi/resources.h");
    @cInclude("uacpi/tables.h");
    @cInclude("uacpi/utilities.h");
    @cInclude("printf/printf.h");
});

const root = @import("root");
const std = @import("std");
const limine = @import("limine");

const apic = @import("./apic.zig");
const arch = @import("./arch.zig");
const mutex = @import("./mutex.zig");
const virt = @import("./virt.zig");
const pci = @import("./pci.zig");
const ps2 = @import("./drivers/ps2.zig");

pub const AcpiTable = struct {
    uacpi_table: *C.uacpi_table,

    pub fn signature(self: *const @This()) [4]u8 {
        return self.uacpi_table.unnamed_0.hdr[0].signature;
    }

    pub fn getData(self: *const @This()) []const u8 {
        const hdr: *C.acpi_sdt_hdr = @ptrCast(self.uacpi_table.unnamed_0.hdr);
        const data: [*]const u8 = @ptrCast(hdr);
        return data[0..hdr.length];
    }
};

fn notifyHandler(
    handle: C.uacpi_handle,
    node: ?*C.uacpi_namespace_node,
    value: C.uacpi_u64,
) callconv(.C) C.uacpi_status {
    _ = handle;
    const path = C.uacpi_namespace_node_generate_absolute_path(node);
    logger.info("Received a notification from {s} {x}", .{ path, value });
    return C.UACPI_STATUS_OK;
}

pub fn init(rsdp_res: *limine.RsdpResponse) !void {
    var init_params: C.uacpi_init_params = .{
        .rsdp = virt.higherHalfToPhysical(rsdp_res.address),
        .no_acpi_mode = false,
        .rt_params = .{
            .log_level = C.UACPI_LOG_TRACE,
            .flags = 0,
        },
    };

    var status: C.uacpi_status = undefined;

    status = C.uacpi_initialize(&init_params);
    if (status != C.UACPI_STATUS_OK) {
        logger.err("Failed to initialize uACPI: {s}", .{C.uacpi_status_to_string(status)});
        return error.InitializationFailed;
    }

    status = C.uacpi_install_notify_handler(C.uacpi_namespace_root(), notifyHandler, null);
    if (status != C.UACPI_STATUS_OK) {
        logger.err("Failed to install notify handler: {s}", .{C.uacpi_status_to_string(status)});
        return error.InitializationFailed;
    }

    status = C.uacpi_namespace_load();
    if (status != C.UACPI_STATUS_OK) {
        logger.err("Failed to load namespace: {s}", .{C.uacpi_status_to_string(status)});
        return error.InitializationFailed;
    }

    status = C.uacpi_namespace_initialize();
    if (status != C.UACPI_STATUS_OK) {
        logger.err("Failed to initialize namespace: {s}", .{C.uacpi_status_to_string(status)});
        return error.InitializationFailed;
    }

    status = C.uacpi_set_interrupt_model(C.UACPI_INTERRUPT_MODEL_IOAPIC);
    if (status != C.UACPI_STATUS_OK) {
        logger.err("Failed to set interrupt model: {s}", .{C.uacpi_status_to_string(status)});
        return error.InitializationFailed;
    }

    const madt_table = findTable(C.ACPI_MADT_SIGNATURE);
    if (madt_table == null) {
        logger.err("Could not find the APIC table", .{});
        return error.TableNotFound;
    }

    var data = madt_table.?.getData()[@sizeOf(C.acpi_madt)..];
    while (data.len >= @sizeOf(C.acpi_entry_hdr)) {
        const entry: *const C.acpi_entry_hdr = @ptrCast(data.ptr);
        switch (entry.type) {
            C.ACPI_MADT_ENTRY_TYPE_IOAPIC => {
                const ioapic: *const C.acpi_madt_ioapic = @ptrCast(entry);
                apic.handleIoApic(ioapic.id, ioapic.address, ioapic.gsi_base);
            },
            C.ACPI_MADT_ENTRY_TYPE_INTERRUPT_SOURCE_OVERRIDE => {
                const iso: *const C.acpi_madt_interrupt_source_override = @ptrCast(entry);
                apic.handleIoApicIso(iso.bus, iso.source, iso.gsi, iso.flags);
            },
            else => logger.warn("Unhandled MADT entry {d} of length {d}", .{ entry.type, entry.length }),
        }
        data = data[entry.length..];
    }
}

pub fn findTable(signature: *const [4]u8) ?AcpiTable {
    var table: ?*C.uacpi_table = null;
    if (C.uacpi_table_find_by_signature(signature, &table) != C.UACPI_STATUS_OK) {
        return null;
    }
    return .{ .uacpi_table = table.? };
}

fn handlePciHostBridge(node: ?*C.uacpi_namespace_node) void {
    const path = C.uacpi_namespace_node_generate_absolute_path(node);
    defer C.uacpi_free_absolute_path(path);

    var status: C.uacpi_status = undefined;
    var segment: u64 = 0;
    var bus: u64 = 0;

    status = C.uacpi_eval_integer(node, "_SEG", null, &segment);
    if (status != C.UACPI_STATUS_OK and status != C.UACPI_STATUS_NOT_FOUND) {
        logger.err("Failed to evaluate _SEG for {s}: {s}", .{ path, C.uacpi_status_to_string(status) });
        return;
    }

    status = C.uacpi_eval_integer(node, "_BBN", null, &bus);
    if (status != C.UACPI_STATUS_OK and status != C.UACPI_STATUS_NOT_FOUND) {
        logger.err("Failed to evaluate _BBN for {s}: {s}", .{ path, C.uacpi_status_to_string(status) });
        return;
    }

    logger.info("Found PCI(e) host bridge {s}, segment {d}, bus {d}", .{ path, segment, bus });
    pci.scanHostBus(@intCast(segment), @intCast(bus)) catch |err| {
        logger.err("An error occurred during host bridge scan {s}: {any}", .{ path, err });
    };
}

fn iterationCallback(
    context: ?*anyopaque,
    node: ?*C.uacpi_namespace_node,
) callconv(.C) C.uacpi_ns_iteration_decision {
    _ = context;

    const pci_pnp_ids: [:null]const ?[*:0]const u8 = &.{ "PNP0A03", "PNP0A08" };
    if (C.uacpi_device_matches_pnp_id(node, @constCast(pci_pnp_ids.ptr))) {
        handlePciHostBridge(node);
    }

    const ps2_kbd_pnp_ids: [:null]const ?[*:0]const u8 = &.{"PNP0303"};
    if (C.uacpi_device_matches_pnp_id(node, @constCast(ps2_kbd_pnp_ids.ptr))) {
        ps2.init(node) catch |err| {
            logger.err("An error occurred during PS/2 keyboard initialization: {any}", .{err});
        };
    }

    return C.UACPI_NS_ITERATION_DECISION_CONTINUE;
}

pub fn enumerateDevices() !void {
    const sb = C.uacpi_namespace_get_predefined(C.UACPI_PREDEFINED_NAMESPACE_SB);
    if (sb == null) {
        logger.err("Failed to get predefined namespace SB", .{});
        return error.NamespaceNotFound;
    }

    C.uacpi_namespace_for_each_node_depth_first(sb, iterationCallback, null);
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
    const pci_device = pci.Device{ .bus = address.bus, .slot = address.device, .function = address.function };
    switch (byte_width) {
        1 => out_value.* = pci_device.read(u8, @intCast(offset)),
        2 => out_value.* = pci_device.read(u16, @intCast(offset)),
        4 => out_value.* = pci_device.read(u32, @intCast(offset)),
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
    const pci_device = pci.Device{ .bus = address.bus, .slot = address.device, .function = address.function };
    switch (byte_width) {
        1 => pci_device.write(u8, @intCast(offset), @truncate(in_value)),
        2 => pci_device.write(u16, @intCast(offset), @truncate(in_value)),
        4 => pci_device.write(u32, @intCast(offset), @truncate(in_value)),
        else => return C.UACPI_STATUS_INVALID_ARGUMENT,
    }
    return C.UACPI_STATUS_OK;
}

export fn uacpi_kernel_io_map(
    base: C.uacpi_io_addr,
    len: C.uacpi_size,
    out_handle: *C.uacpi_handle,
) callconv(.C) C.uacpi_status {
    _ = len;
    out_handle.* = @ptrFromInt(base);
    return C.UACPI_STATUS_OK;
}

export fn uacpi_kernel_io_unmap(handle: C.uacpi_handle) callconv(.C) void {
    _ = handle;
}

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

export fn uacpi_kernel_map(addr: C.uacpi_phys_addr, len: C.uacpi_size) callconv(.C) *anyopaque {
    _ = len;
    return virt.asHigherHalf(*anyopaque, addr);
}

export fn uacpi_kernel_unmap(addr: *anyopaque, len: C.uacpi_size) callconv(.C) void {
    _ = addr;
    _ = len;
}

export fn uacpi_kernel_alloc(size: C.uacpi_size) callconv(.C) *anyopaque {
    const result = root.allocator.rawAlloc(size, 8, @returnAddress());

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
        root.allocator.rawFree(result[0..size], 8, @returnAddress());
    }
}

export fn uacpi_kernel_log(level: C.uacpi_log_level, fmt: [*c]const C.uacpi_u8, ...) callconv(.C) void {
    var ap = @cVaStart();
    defer @cVaEnd(&ap);
    uacpi_kernel_vlog(level, fmt, &ap);
}

export fn uacpi_kernel_vlog(
    level: C.uacpi_log_level,
    fmt: [*c]const C.uacpi_u8,
    args: *std.builtin.VaList,
) callconv(.C) void {
    var buffer: [256]u8 = undefined;
    var length: usize = @intCast(C.vsnprintf_(&buffer, buffer.len, fmt, @ptrCast(args)));

    // zig std logging inserts its own newline
    if (buffer[length - 1] == '\n') {
        buffer[length - 1] = 0;
        length -= 1;
    }

    const level_str = switch (level) {
        C.UACPI_LOG_TRACE => "trace",
        C.UACPI_LOG_INFO => "info",
        C.UACPI_LOG_WARN => "warn",
        C.UACPI_LOG_ERROR => "error",
        else => "invalid",
    };

    logger.info("uacpi-{s}: {s}", .{ level_str, buffer[0..length] });
}

export fn uacpi_kernel_get_ticks() callconv(.C) C.uacpi_u64 {
    logger.warn("uacpi_kernel_get_ticks is a stub", .{});
    return 0;
}

export fn uacpi_kernel_stall(usec: C.uacpi_u8) callconv(.C) void {
    _ = usec;
    logger.warn("uacpi_kernel_stall is a stub", .{});
}

export fn uacpi_kernel_sleep(msec: C.uacpi_u64) callconv(.C) void {
    _ = msec;
    logger.warn("uacpi_kernel_sleep is a stub", .{});
}

export fn uacpi_kernel_create_mutex() callconv(.C) C.uacpi_handle {
    const result = root.allocator.create(mutex.AtomicMutex) catch {
        @panic("uacpi_kernel_create_mutex failed");
    };

    result.* = .{};
    return @ptrCast(result);
}

export fn uacpi_kernel_free_mutex(handle: C.uacpi_handle) callconv(.C) void {
    if (handle) |ptr| {
        const mtx: *mutex.AtomicMutex = @ptrCast(@alignCast(ptr));
        root.allocator.destroy(mtx);
    }
}

export fn uacpi_kernel_acquire_mutex(
    handle: C.uacpi_handle,
    timeout: C.uacpi_u16,
) callconv(.C) C.uacpi_bool {
    const mtx: *mutex.AtomicMutex = @ptrCast(@alignCast(handle));

    if (timeout != 0xFFFF) {
        logger.warn("uacpi_kernel_acquire_mutex does not support timeout", .{});
    }

    mtx.lock();

    return true;
}

export fn uacpi_kernel_release_mutex(handle: C.uacpi_handle) callconv(.C) void {
    const mtx: *mutex.AtomicMutex = @ptrCast(@alignCast(handle));

    mtx.unlock();
}

export fn uacpi_kernel_create_event() callconv(.C) C.uacpi_handle {
    logger.warn("uacpi_kernel_create_event is a stub", .{});
    unreachable;
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

export fn uacpi_kernel_install_interrupt_handler(
    irq: C.uacpi_u32,
    handler: C.uacpi_interrupt_handler,
    ctx: C.uacpi_handle,
    out_irq_handle: *C.uacpi_handle,
) callconv(.C) C.uacpi_status {
    _ = irq;
    _ = handler;
    _ = ctx;
    _ = out_irq_handle;
    logger.warn("uacpi_kernel_install_interrupt_handler is a stub", .{});
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

export fn uacpi_kernel_free_spinlock(lock: C.uacpi_handle) callconv(.C) void {
    uacpi_kernel_free_mutex(lock);
}

export fn uacpi_kernel_spinlock_lock(lock: C.uacpi_handle) callconv(.C) C.uacpi_cpu_flags {
    _ = uacpi_kernel_acquire_mutex(lock, 0xFFFF);
    return 0;
}

export fn uacpi_kernel_spinlock_unlock(lock: C.uacpi_handle, flags: C.uacpi_cpu_flags) callconv(.C) void {
    _ = flags;
    uacpi_kernel_release_mutex(lock);
}

export fn uacpi_kernel_schedule_work(
    work_type: C.uacpi_work_type,
    handler: C.uacpi_work_handler,
    ctx: C.uacpi_handle,
) callconv(.C) C.uacpi_status {
    _ = work_type;
    logger.warn("uacpi_kernel_schedule_work is a stub", .{});

    if (handler) |handler_| {
        handler_(ctx);
    } else {
        logger.warn("uacpi_kernel_schedule_work: handler is null", .{});
        return C.UACPI_STATUS_INVALID_ARGUMENT;
    }

    return C.UACPI_STATUS_OK;
}

export fn uacpi_kernel_wait_for_work_completion() callconv(.C) C.uacpi_status {
    logger.warn("uacpi_kernel_wait_for_work_completion is a stub", .{});
    return C.UACPI_STATUS_OK;
}

export fn strlen(s: [*]const c_char) callconv(.C) usize {
    var len: usize = 0;

    while (s[len] != 0) {
        len += 1;
    }

    return len;
}

export fn strcmp(s1: [*]const c_char, s2: [*]const c_char) callconv(.C) i32 {
    var i: usize = 0;

    while (s1[i] != 0 and s2[i] != 0) {
        if (s1[i] != s2[i]) {
            return s1[i] - s2[i];
        }

        i += 1;
    }

    return s1[i] - s2[i];
}

export fn strnlen(s: [*]const c_char, maxlen: usize) callconv(.C) usize {
    var len: usize = 0;

    while (len < maxlen) {
        if (s[len] == 0) {
            break;
        }

        len += 1;
    }

    return len;
}

export fn strncmp(s1: [*]const c_char, s2: [*]const c_char, n: usize) callconv(.C) i32 {
    for (0..n) |i| {
        if (s1[i] == 0 or s2[i] == 0) {
            return s1[i] - s2[i];
        }

        if (s1[i] != s2[i]) {
            return s1[i] - s2[i];
        }
    }

    return 0;
}
