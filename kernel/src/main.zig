const logger = std.log.scoped(.main);

const C = @cImport({
    @cInclude("flanterm.h");
    @cInclude("backends/fb.h");
});

const std = @import("std");
const limine = @import("limine");

const acpi = @import("./acpi.zig");
const apic = @import("./apic.zig");
const arch = @import("./arch.zig");
const debug = @import("./debug.zig");
const hpet = @import("./hpet.zig");
const interrupts = @import("./interrupts.zig");
const lock = @import("./lock.zig");
const per_cpu = @import("./per_cpu.zig");
const pci = @import("./pci.zig");
const phys = @import("./phys.zig");
const scheduler = @import("./scheduler.zig");
const time = @import("./time.zig");
const utils = @import("./utils.zig");
const vfs = @import("./vfs.zig");
const virt = @import("./virt.zig");

const PageAllocator = struct {
    pub fn allocate(_: *@This(), pages: usize) ?u64 {
        return kernel_paged_arena.allocate(pages * std.mem.page_size);
    }

    fn alloc(ctx: *anyopaque, len: usize, _: u8, _: usize) ?[*]u8 {
        const self = @as(*PageAllocator, @ptrCast(@alignCast(ctx)));
        const pages = std.math.divCeil(usize, len, std.mem.page_size) catch unreachable;
        return @ptrFromInt(self.allocate(pages) orelse return null);
    }

    fn resize(_: *anyopaque, _: []u8, _: u8, _: usize, _: usize) bool {
        return false;
    }

    fn free(_: *anyopaque, _: []u8, _: u8, _: usize) void {}
};

var flanterm_ctx: ?*C.flanterm_context = null;
var print_lock: lock.Spinlock = .{};

pub const log_level = std.log.Level.debug;

pub const os = struct {
    pub const system = struct {};

    pub const heap = struct {
        pub const page_allocator = std.mem.Allocator{
            .ptr = &page_heap_allocator,
            .vtable = &std.mem.Allocator.VTable{
                .alloc = PageAllocator.alloc,
                .resize = PageAllocator.resize,
                .free = PageAllocator.free,
            },
        };
    };
};

pub const std_options: std.Options = .{ .logFn = log };

pub var kernel_va_arena: virt.Arena = .{};
pub var kernel_paged_arena: virt.Arena = .{};

pub var page_heap_allocator: PageAllocator = .{};
pub var gp_allocator = std.heap.GeneralPurposeAllocator(.{ .thread_safe = true, .MutexType = lock.Spinlock }){};
pub var allocator = gp_allocator.allocator();

pub export var boot_info_req: limine.BootloaderInfoRequest = .{};
pub export var hhdm_req: limine.HhdmRequest = .{};
pub export var memory_map_req: limine.MemoryMapRequest = .{};
pub export var modules_req: limine.ModuleRequest = .{};
pub export var kernel_file_req: limine.KernelFileRequest = .{};
pub export var rsdp_req: limine.RsdpRequest = .{};
pub export var kernel_addr_req: limine.KernelAddressRequest = .{};
pub export var framebuffer_req: limine.FramebufferRequest = .{};
pub export var boot_time_req: limine.BootTimeRequest = .{};

export fn _start() callconv(.C) noreturn {
    main() catch |err| {
        logger.err("Failed to initialize: {any}", .{err});

        if (@errorReturnTrace()) |stack_trace| {
            debug.printStackTrace(stack_trace);
        }
    };

    while (true) {
        arch.halt();
    }
}

fn mainThread(_: u8) !void {
    try pci.init();
    try acpi.enumerateDevices();

    // const process = try scheduler.spawnProcess(null);
    // const thread = try scheduler.spawnThread(process);
    // const init = try vfs.resolve(null, "/usr/bin/init", 0);

    // try thread.exec(init, &.{"/usr/bin/init"}, &.{});

    // scheduler.enqueue(thread);

    while (true) {
        scheduler.yield();
    }
}

fn pagedAlloc(source: *virt.Arena, size: usize) ?u64 {
    const page_table = virt.kernel_address_space.page_table;
    const pages = @divExact(size, std.mem.page_size);
    const address = source.allocate(pages * std.mem.page_size) orelse return null;
    for (0..pages) |i| {
        const page = phys.allocate(1, .conventional) orelse return null;
        page_table.mapPage(
            address + i * std.mem.page_size,
            page,
            virt.PTEFlags.present | virt.PTEFlags.writable,
        ) catch return null;
    }
    return address;
}

fn pagedFree(source: *virt.Arena, address: u64, size: usize) void {
    const page_table = virt.kernel_address_space.page_table;
    const pages = std.math.divCeil(usize, size, std.mem.page_size) catch unreachable;
    for (0..pages) |i| {
        const addr = address + i * std.mem.page_size;
        const phys_addr = page_table.translate(addr) orelse unreachable;
        page_table.unmapPage(addr) catch unreachable;
        phys.free(phys_addr, 1);
    }
    source.free(address, size);
}

fn main() !void {
    asm volatile ("cli");
    defer asm volatile ("sti");

    per_cpu.initBsp();
    per_cpu.initFeatures();

    virt.bootstrapArena();

    kernel_va_arena = virt.Arena.init("kernel-va", 0xFFFF_A000_0000_0000, utils.tib(16));
    kernel_paged_arena = virt.Arena.initWithSource("kernel-paged", &kernel_va_arena, pagedAlloc, pagedFree);

    const boot_info_res = boot_info_req.response.?;
    const hhdm_res = hhdm_req.response.?;
    const memory_map_res = memory_map_req.response.?;
    const kernel_addr_res = kernel_addr_req.response.?;
    const modules_res = modules_req.response.?;
    const rsdp_res = rsdp_req.response.?;
    const framebuffer_res = framebuffer_req.response.?;

    const framebuffer = framebuffer_res.framebuffers()[0];
    flanterm_ctx = C.flanterm_fb_init(null, null, @ptrCast(@alignCast(framebuffer.address)), //
        framebuffer.width, framebuffer.height, framebuffer.pitch, framebuffer.red_mask_size, framebuffer.red_mask_shift, //
        framebuffer.green_mask_size, framebuffer.green_mask_shift, framebuffer.blue_mask_size, framebuffer.blue_mask_shift, //
        null, null, null, null, null, null, null, null, 0, 0, 1, 0, 0, 0);

    if (kernel_file_req.response) |res| {
        debug.init(res) catch |err|
            logger.warn("Failed to parsee debug information: {any}", .{err});
    }

    if (boot_time_req.response) |res| {
        time.init(res);
    }

    std.debug.assert(hhdm_res.offset == virt.asHigherHalf(u64, 0));
    logger.info("Booted using {s} {s}", .{ boot_info_res.name, boot_info_res.version });

    try phys.init(memory_map_res);
    try virt.init(memory_map_res, kernel_addr_res);

    try per_cpu.init();
    try acpi.init(rsdp_res);
    try hpet.init(false);
    apic.init();

    try vfs.init(modules_res);
    try scheduler.init();
    _ = try scheduler.startKernelThread(mainThread, 0);
}

pub fn panic(msg: []const u8, error_return_trace: ?*std.builtin.StackTrace, ret_addr: ?usize) noreturn {
    asm volatile ("cli");
    _ = error_return_trace;
    _ = ret_addr;
    logger.err("Kernel panic: {s}", .{msg});
    debug.printStackIterator(std.debug.StackIterator.init(@returnAddress(), @frameAddress()));
    while (true) {
        arch.halt();
    }
}

const LogWriter = struct {
    pub const Error = error{};

    pub fn write(_: @This(), bytes: []const u8) Error!usize {
        debug.debugPrint(bytes);
        if (flanterm_ctx) |ctx| {
            C.flanterm_write(ctx, bytes.ptr, bytes.len);
        }
        return bytes.len;
    }

    pub fn writeByte(self: @This(), byte: u8) Error!void {
        _ = try self.write(&.{byte});
    }

    pub fn writeBytesNTimes(self: @This(), bytes: []const u8, n: usize) Error!void {
        for (0..n) |_| {
            _ = try self.write(bytes);
        }
    }

    pub fn writeAll(self: @This(), bytes: []const u8) Error!void {
        _ = try self.write(bytes);
    }
};

pub fn log(
    comptime level: std.log.Level,
    comptime scope: anytype,
    comptime fmt: []const u8,
    args: anytype,
) void {
    print_lock.lock();
    defer print_lock.unlock();
    const current_time = time.getClock(.monotonic);
    std.fmt.format(
        @as(LogWriter, undefined),
        "[{d}.{d:0>6}] [CPU{d}] {s}({s}): " ++ fmt ++ "\n",
        .{
            current_time.seconds,
            @as(u64, @intCast(current_time.nanoseconds)) / std.time.ns_per_us,
            per_cpu.get().lapic_id,
            @tagName(level),
            @tagName(scope),
        } ++ args,
    ) catch unreachable;
}

export fn putchar_(ch: u8) callconv(.C) void {
    _ = ch;
    @panic("This function should never be called!");
}
