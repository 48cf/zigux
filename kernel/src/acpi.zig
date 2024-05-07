const logger = std.log.scoped(.acpi);

const C = @cImport({
    @cInclude("uacpi/uacpi.h");
    @cInclude("uacpi/acpi.h");
    @cInclude("uacpi/notify.h");
    @cInclude("uacpi/tables.h");
    @cInclude("printf/printf.h");
});

const root = @import("root");
const std = @import("std");
const limine = @import("limine");

const apic = @import("./apic.zig");
const arch = @import("./arch.zig");
const mutex = @import("./mutex.zig");
const virt = @import("./virt.zig");

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

fn handle_notify(
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

    _ = C.uacpi_initialize(&init_params);
    _ = C.uacpi_install_notify_handler(C.uacpi_namespace_root(), handle_notify, null);
    _ = C.uacpi_namespace_load();
    _ = C.uacpi_namespace_initialize();

    const madt_table = findTable(C.ACPI_MADT_SIGNATURE);
    if (madt_table == null) {
        logger.err("APIC table not found", .{});
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

export fn uacpi_kernel_raw_memory_read(
    address: C.uacpi_phys_addr,
    byte_width: C.uacpi_u8,
    out_value: *C.uacpi_u64,
) callconv(.C) C.uacpi_status {
    logger.debug("uacpi_kernel_raw_memory_read: address={x} byte_width={d}", .{ address, byte_width });

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
    logger.debug("uacpi_kernel_raw_memory_write: address={x} byte_width={d} value={x}", .{ address, byte_width, in_value });

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

    logger.debug("uacpi_kernel_raw_io_read: port={x} byte_width={d}", .{ port, byte_width });

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

    logger.debug("uacpi_kernel_raw_io_write: port={x} byte_width={d} value={x}", .{ port, byte_width, in_value });

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
    _ = address;
    _ = offset;
    switch (byte_width) {
        1 => out_value.* = 0xFF,
        2 => out_value.* = 0xFFFF,
        4 => out_value.* = 0xFFFFFFFF,
        else => return C.UACPI_STATUS_INVALID_ARGUMENT,
    }
    logger.warn("uacpi_kernel_pci_read is a stub", .{});
    return C.UACPI_STATUS_OK;
}

export fn uacpi_kernel_pci_write(
    address: *C.uacpi_pci_address,
    offset: C.uacpi_size,
    byte_width: C.uacpi_u8,
    in_value: C.uacpi_u64,
) callconv(.C) C.uacpi_status {
    _ = address;
    _ = offset;
    _ = byte_width;
    _ = in_value;
    logger.warn("uacpi_kernel_pci_write is a stub", .{});
    return C.UACPI_STATUS_OK;
}

export fn uacpi_kernel_io_map(
    base: C.uacpi_io_addr,
    len: C.uacpi_size,
    out_handle: *C.uacpi_handle,
) callconv(.C) C.uacpi_status {
    _ = base;
    _ = len;
    out_handle.* = null;
    logger.warn("uacpi_kernel_io_map is a stub", .{});
    return C.UACPI_STATUS_OK;
}

export fn uacpi_kernel_io_unmap(handle: C.uacpi_handle) callconv(.C) C.uacpi_status {
    _ = handle;
    logger.warn("uacpi_kernel_io_unmap is a stub", .{});
    return C.UACPI_STATUS_OK;
}

export fn uacpi_kernel_io_read(
    handle: C.uacpi_handle,
    offset: C.uacpi_size,
    byte_width: C.uacpi_u8,
    out_value: *C.uacpi_u64,
) callconv(.C) C.uacpi_status {
    _ = handle;
    _ = offset;
    switch (byte_width) {
        1 => out_value.* = 0xFF,
        2 => out_value.* = 0xFFFF,
        4 => out_value.* = 0xFFFFFFFF,
        else => return C.UACPI_STATUS_INVALID_ARGUMENT,
    }
    logger.warn("uacpi_kernel_io_read is a stub", .{});
    return C.UACPI_STATUS_OK;
}

export fn uacpi_kernel_io_write(
    handle: C.uacpi_handle,
    offset: C.uacpi_size,
    byte_width: C.uacpi_u8,
    in_value: C.uacpi_u64,
) callconv(.C) C.uacpi_status {
    _ = handle;
    _ = offset;
    _ = byte_width;
    _ = in_value;
    logger.warn("uacpi_kernel_io_write is a stub", .{});
    return C.UACPI_STATUS_OK;
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

export fn strlen(s: [*]const u8) callconv(.C) usize {
    var len: usize = 0;

    while (s[len] != 0) {
        len += 1;
    }

    return len;
}

export fn strcmp(s1: [*]const u8, s2: [*]const u8) callconv(.C) i32 {
    var i: usize = 0;

    while (s1[i] != 0 and s2[i] != 0) {
        if (s1[i] != s2[i]) {
            return s1[i] - s2[i];
        }

        i += 1;
    }

    return s1[i] - s2[i];
}

export fn strnlen(s: [*]const u8, maxlen: usize) callconv(.C) usize {
    var len: usize = 0;

    while (len < maxlen) {
        if (s[len] == 0) {
            break;
        }

        len += 1;
    }

    return len;
}

export fn strncmp(s1: [*]const u8, s2: [*]const u8, n: usize) callconv(.C) i32 {
    for (0..n) |i| {
        if (s1[i] != s2[i]) {
            if (s1[i] > s2[i]) {
                return 1;
            } else {
                return -1;
            }
        }

        if (s1[i] == 0) {
            return -1;
        }
    }

    return 0;
}
