const logger = std.log.scoped(.main);

const std = @import("std");

const apic = @import("apic.zig");
const acpi = @import("acpi.zig");
const arch = @import("arch.zig");
const interrupts = @import("interrupts.zig");
const debug = @import("debug.zig");
const limine = @import("limine.zig");
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

// TODO: Demand page that bitch :^)
const PageAllocator = struct {
    bump: u64 = 0xFFFF_A000_0000_0000,

    const heap_logger = std.log.scoped(.heap);

    fn alloc(ptr: *anyopaque, len: usize, ptr_align: u29, len_align: u29, ret_addr: usize) std.mem.Allocator.Error![]u8 {
        _ = ptr_align;
        _ = len_align;
        _ = ret_addr;

        const pages = utils.alignUp(usize, len, std.mem.page_size) / std.mem.page_size;
        const self = @ptrCast(*PageAllocator, @alignCast(8, ptr));
        const base = self.bump;

        if (phys.freePages() < pages) {
            return error.OutOfMemory;
        }

        var i: usize = 0;

        while (i < pages) : (i += 1) {
            const page = phys.allocate(1, true) orelse return error.OutOfMemory;

            virt.kernel_address_space.page_table.mapPage(
                base + i * std.mem.page_size,
                page,
                virt.Flags.Present | virt.Flags.Writable,
            ) catch return error.OutOfMemory;
        }

        self.bump += pages * std.mem.page_size;

        return @ptrCast([*]u8, @intToPtr(*u8, base))[0..len];
    }

    fn resize(ptr: *anyopaque, buf: []u8, buf_align: u29, new_len: usize, len_align: u29, ret_addr: usize) ?usize {
        _ = ptr;
        _ = buf;
        _ = buf_align;
        _ = new_len;
        _ = len_align;
        _ = ret_addr;

        return null;
    }

    fn free(ptr: *anyopaque, buf: []u8, buf_align: u29, ret_addr: usize) void {
        _ = ptr;
        _ = buf;
        _ = buf_align;
        _ = ret_addr;
    }
};

var page_allocator = PageAllocator{};
var print_lock: IrqSpinlock = .{};

pub const log_level = std.log.Level.debug;
pub const os = .{
    .heap = .{
        .page_allocator = std.mem.Allocator{
            .ptr = &page_allocator,
            .vtable = &std.mem.Allocator.VTable{
                .alloc = PageAllocator.alloc,
                .resize = PageAllocator.resize,
                .free = PageAllocator.free,
            },
        },
    },
};

pub var gp_allocator = std.heap.GeneralPurposeAllocator(.{ .thread_safe = true, .MutexType = mutex.AtomicMutex }){};
pub var allocator = gp_allocator.allocator();

pub export var boot_info_req: limine.BootloaderInfo.Request = .{ .revision = 0 };
pub export var hhdm_req: limine.Hhdm.Request = .{ .revision = 0 };
pub export var term_req: limine.Terminal.Request = .{ .revision = 0 };
pub export var memory_map_req: limine.MemoryMap.Request = .{ .revision = 0 };
pub export var modules_req: limine.Modules.Request = .{ .revision = 0 };
pub export var kernel_file_req: limine.KernelFile.Request = .{ .revision = 0 };
pub export var rsdp_req: limine.Rsdp.Request = .{ .revision = 0 };
pub export var kernel_addr_req: limine.KernelAddress.Request = .{ .revision = 0 };

export fn platformMain() noreturn {
    main() catch |err| {
        logger.err("Failed to initialize: {e}", .{err});

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
    const bash = try vfs.resolve(null, "/bin/sh", 0);

    try thread.exec(bash, &.{"/bin/sh"}, &.{ "TERM=linux", "HOME=/root", "PATH=/usr/bin" });

    scheduler.enqueue(thread);
}

pub fn panic(message: []const u8, stack_trace: ?*std.builtin.StackTrace) noreturn {
    asm volatile ("cli");

    _ = stack_trace;

    logger.err("Kernel panic: {s}", .{message});

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
    _ = print_lock.lock();

    defer print_lock.unlock();

    var buffer = std.io.fixedBufferStream(&bytes);
    var writer = buffer.writer();

    writer.print("{s}({s}): ", .{ @tagName(level), @tagName(scope) }) catch unreachable;
    writer.print(fmt ++ "\n", args) catch unreachable;
    writer.writeByte(0) catch unreachable;

    debug.debugPrint(std.mem.span(@ptrCast([*:0]const u8, &bytes)));
}
