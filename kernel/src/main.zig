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
const mutex = @import("./mutex.zig");
const per_cpu = @import("./per_cpu.zig");
const phys = @import("./phys.zig");
const scheduler = @import("./scheduler.zig");
const time = @import("./time.zig");
const vfs = @import("./vfs.zig");
const virt = @import("./virt.zig");

const PageAllocator = struct {
    bump: std.atomic.Value(u64) = .{ .raw = 0xFFFF_A000_0000_0000 },

    pub fn allocate(self: *@This(), pages: usize) ?u64 {
        const base = self.bump.fetchAdd((pages + 1) * std.mem.page_size, .acq_rel);
        for (0..pages) |i| {
            const page = phys.allocate(1, true) orelse return null;
            virt.kernel_address_space.page_table.mapPage(
                base + (i + 1) * std.mem.page_size,
                page,
                virt.PTEFlags.present | virt.PTEFlags.writable,
            ) catch return null;
        }
        return base + std.mem.page_size;
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

fn flantermAlloc(size: usize) callconv(.C) ?*anyopaque {
    const result = allocator.alignedAlloc(u8, 8, size) catch return null;
    return result.ptr;
}

fn flantermFree(addr: ?*anyopaque, size: usize) callconv(.C) void {
    if (addr) |ptr| {
        allocator.free(@as([*]u8, @ptrCast(ptr))[0..size]);
    }
}

fn main() !void {
    asm volatile ("cli");
    defer asm volatile ("sti");

    const boot_info_res = boot_info_req.response.?;
    const hhdm_res = hhdm_req.response.?;
    const memory_map_res = memory_map_req.response.?;
    const kernel_addr_res = kernel_addr_req.response.?;
    const modules_res = modules_req.response.?;
    const rsdp_res = rsdp_req.response.?;
    const framebuffer_res = framebuffer_req.response.?;

    if (kernel_file_req.response) |res| {
        debug.init(res) catch |err|
            logger.warn("Failed to parsee debug information: {any}", .{err});
    }

    if (boot_time_req.response) |res| {
        time.init(res);
    }

    per_cpu.initBsp();
    per_cpu.initFeatures();

    std.debug.assert(hhdm_res.offset == virt.asHigherHalf(u64, 0));
    logger.info("Booted using {s} {s}", .{ boot_info_res.name, boot_info_res.version });

    try phys.init(memory_map_res);
    try virt.init(kernel_addr_res);

    const framebuffer = framebuffer_res.framebuffers()[0];

    flanterm_ctx = C.flanterm_fb_init(flantermAlloc, flantermFree, @ptrCast(@alignCast(framebuffer.address)), //
        framebuffer.width, framebuffer.height, framebuffer.pitch, framebuffer.red_mask_size, framebuffer.red_mask_shift, //
        framebuffer.green_mask_size, framebuffer.green_mask_shift, framebuffer.blue_mask_size, framebuffer.blue_mask_shift, //
        null, null, null, null, null, null, null, null, 0, 0, 1, 0, 0, 0);

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
