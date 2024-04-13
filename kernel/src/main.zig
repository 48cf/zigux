const logger = std.log.scoped(.main);

const std = @import("std");
const limine = @import("limine");

const apic = @import("apic.zig");
const acpi = @import("acpi.zig");
const arch = @import("arch.zig");
const interrupts = @import("interrupts.zig");
const debug = @import("debug.zig");
const mutex = @import("mutex.zig");
const per_cpu = @import("per_cpu.zig");
const pci = @import("pci.zig");
const phys = @import("phys.zig");
const utils = @import("utils.zig");
const scheduler = @import("scheduler.zig");
const virt = @import("virt.zig");
const vfs = @import("vfs.zig");
const ps2 = @import("drivers/ps2.zig");

const IrqSpinlock = @import("irq_lock.zig").IrqSpinlock;

const PageAllocator = struct {
    bump: u64 = 0xFFFF_A000_0000_0000,

    const heap_logger = std.log.scoped(.heap);

    fn alloc(ctx: *anyopaque, len: usize, ptr_align: u8, ret_addr: usize) ?[*]u8 {
        _ = ptr_align;
        _ = ret_addr;

        const pages = std.mem.alignForward(usize, len, std.mem.page_size) / std.mem.page_size;
        const self = @as(*PageAllocator, @ptrCast(@alignCast(ctx)));
        const base = self.bump;

        if (phys.freePages() < pages) {
            return null;
        }

        var i: usize = 0;

        while (i < pages) : (i += 1) {
            const page = phys.allocate(1, true) orelse return null;

            virt.kernel_address_space.page_table.mapPage(
                base + i * std.mem.page_size,
                page,
                virt.Flags.Present | virt.Flags.Writable,
            ) catch return null;
        }

        self.bump += pages * std.mem.page_size;

        return @as([*]u8, @ptrCast(@as(*u8, @ptrFromInt(base))));
    }

    fn resize(ctx: *anyopaque, buf: []u8, buf_align: u8, new_len: usize, ret_addr: usize) bool {
        _ = ctx;
        _ = buf;
        _ = buf_align;
        _ = new_len;
        _ = ret_addr;

        return false;
    }

    fn free(ctx: *anyopaque, buf: []u8, buf_align: u8, ret_addr: usize) void {
        _ = ctx;
        _ = buf;
        _ = buf_align;
        _ = ret_addr;
    }
};

var page_heap_allocator = PageAllocator{};
var print_lock: IrqSpinlock = .{};

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

pub const std_options = .{ .logFn = log };

pub var gp_allocator = std.heap.GeneralPurposeAllocator(.{ .thread_safe = true, .MutexType = IrqSpinlock }){};
pub var allocator = gp_allocator.allocator();

pub export var boot_info_req: limine.BootloaderInfoRequest = .{};
pub export var hhdm_req: limine.HhdmRequest = .{};
pub export var term_req: limine.TerminalRequest = .{};
pub export var memory_map_req: limine.MemoryMapRequest = .{};
pub export var modules_req: limine.ModuleRequest = .{};
pub export var kernel_file_req: limine.KernelFileRequest = .{};
pub export var rsdp_req: limine.RsdpRequest = .{};
pub export var kernel_addr_req: limine.KernelAddressRequest = .{};

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

fn main() !void {
    asm volatile ("cli");
    defer asm volatile ("sti");

    const boot_info_res = boot_info_req.response.?;
    const hhdm_res = hhdm_req.response.?;
    const memory_map_res = memory_map_req.response.?;
    const kernel_addr_res = kernel_addr_req.response.?;
    const modules_res = modules_req.response.?;
    const rsdp_res = rsdp_req.response.?;

    per_cpu.initFeatures();

    asm volatile ("wrgsbase %[base]"
        :
        : [base] "r" (@as(u64, 0)),
    );

    std.debug.assert(hhdm_res.offset == virt.asHigherHalf(u64, 0));
    logger.info("Booted using {s} {s}", .{ boot_info_res.name, boot_info_res.version });

    try phys.init(memory_map_res);
    try virt.init(kernel_addr_res);
    try per_cpu.init();
    try vfs.init(modules_res);
    try acpi.init(rsdp_res);
    try pci.init();
    try scheduler.init();

    apic.init();
    apic.initTimer();

    ps2.init();

    const process = try scheduler.spawnProcess(null);
    const thread = try scheduler.spawnThread(process);
    const init = try vfs.resolve(null, "/usr/bin/init", 0);

    try thread.exec(init, &.{"/usr/bin/init"}, &.{});

    scheduler.enqueue(thread);
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

var bytes: [16 * 4096]u8 = undefined;

pub fn log(
    comptime level: std.log.Level,
    comptime scope: anytype,
    comptime fmt: []const u8,
    args: anytype,
) void {
    print_lock.lock();
    defer print_lock.unlock();

    var buffer = std.io.fixedBufferStream(&bytes);
    var writer = buffer.writer();

    const tid = if (per_cpu.tryGet()) |cpu_info| blk: {
        break :blk if (cpu_info.thread) |thread| thread.tid else 0;
    } else 0;

    writer.print("[{d:>3}] ({s}) {s}: ", .{ tid, @tagName(scope), @tagName(level) }) catch unreachable;
    writer.print(fmt ++ "\n", args) catch unreachable;

    debug.debugPrint(buffer.getWritten());
}
