const logger = std.log.scoped(.main);

const std = @import("std");

const apic = @import("apic.zig");
const arch = @import("arch.zig");
const debug = @import("debug.zig");
const limine = @import("limine.zig");
const per_cpu = @import("per_cpu.zig");
const phys = @import("phys.zig");
const utils = @import("utils.zig");
const IrqSpinlock = @import("irq_lock.zig").IrqSpinlock;
const scheduler = @import("scheduler.zig");
const virt = @import("virt.zig");

const PageAllocator = struct {
    bump: u64 = 0xFFFF900000000000,

    const heap_logger = std.log.scoped(.heap);

    fn alloc(ptr: *anyopaque, len: usize, ptr_align: u29, len_align: u29, ret_addr: usize) std.mem.Allocator.Error![]u8 {
        _ = ptr_align;
        _ = len_align;
        _ = ret_addr;

        const pages = utils.align_up(usize, len, std.mem.page_size) / std.mem.page_size;
        const self = @ptrCast(*PageAllocator, @alignCast(8, ptr));
        const base = self.bump;

        if (phys.free_pages() < pages) {
            return error.OutOfMemory;
        }

        heap_logger.debug("Page allocator asked for {} pages (size={})", .{ pages, len });

        var i: usize = 0;

        while (i < pages) : (i += 1) {
            const page = phys.allocate(1, true) orelse return error.OutOfMemory;

            heap_logger.debug("Mapping heap page 0x{X:0>16} at 0x{X:0>16}", .{ page, base + i * std.mem.page_size });

            virt.kernel_page_table.mapPage(
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

        const pages = utils.align_up(usize, buf.len, std.mem.page_size) / std.mem.page_size;
        const base = @ptrToInt(buf.ptr);

        heap_logger.debug("Attempt to free {} pages at 0x{X:0>16} (size={})", .{ pages, base, buf.len });
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

pub var gp_allocator = std.heap.GeneralPurposeAllocator(.{ .thread_safe = true, .MutexType = std.Thread.Mutex.AtomicMutex }){};
pub var allocator = gp_allocator.allocator();

pub export var boot_info_req: limine.BootloaderInfo.Request = .{ .revision = 0 };
pub export var hhdm_req: limine.Hhdm.Request = .{ .revision = 0 };
pub export var term_req: limine.Terminal.Request = .{ .revision = 0 };
pub export var memory_map_req: limine.MemoryMap.Request = .{ .revision = 0 };
pub export var modules_req: limine.Modules.Request = .{ .revision = 0 };
pub export var kernel_file_req: limine.KernelFile.Request = .{ .revision = 0 };
pub export var rsdp_req: limine.Rsdp.Request = .{ .revision = 0 };
pub export var kernel_addr_req: limine.KernelAddress.Request = .{ .revision = 0 };

export fn platform_main() noreturn {
    main() catch |err| {
        logger.err("Failed to initialize: {e}", .{err});
    };

    while (true) {
        arch.halt();
    }
}

fn main() !void {
    const boot_info_res = boot_info_req.response.?;
    const hhdm_res = hhdm_req.response.?;
    const memory_map_res = memory_map_req.response.?;
    const kernel_addr_res = kernel_addr_req.response.?;

    logger.info("Booted using {s} {s}", .{ boot_info_res.name, boot_info_res.version });

    // var cr4 = asm volatile ("mov %%cr4, %[cr4]"
    //     : [cr4] "=r" (-> u64),
    // );

    // cr4 |= 1 << 20;
    // cr4 |= 1 << 21;

    // asm volatile ("mov %[cr4], %%cr4"
    //     :
    //     : [cr4] "r" (cr4),
    // );

    try phys.init(memory_map_res, hhdm_res);
    try virt.init(hhdm_res, kernel_addr_res);
    try per_cpu.init();
    try scheduler.init();

    apic.init();
    apic.init_timer();

    asm volatile ("sti");
}

const assemblerElf align(8) = @embedFile("../s3").*;

pub fn mainThread() noreturn {
    const assemblerProcess = scheduler.spawnProcess(null) catch unreachable;
    const assemblerThread = scheduler.spawnThread(assemblerProcess) catch unreachable;

    assemblerThread.exec(&assemblerElf) catch unreachable;
    scheduler.enqueue(assemblerThread);

    std.os.linux.exit(0);
}

pub fn panic(message: []const u8, stack_trace: ?*std.builtin.StackTrace) noreturn {
    _ = stack_trace;

    logger.err("Kernel panic: {s}", .{message});

    debug.printStackIterator(std.debug.StackIterator.init(@returnAddress(), @frameAddress()));

    while (true) {
        arch.halt();
    }
}

pub fn log(
    comptime level: std.log.Level,
    comptime scope: anytype,
    comptime fmt: []const u8,
    args: anytype,
) void {
    print_lock.lock();

    defer print_lock.unlock();

    var bytes: [1024]u8 = undefined;
    var buffer = std.io.fixedBufferStream(&bytes);
    var writer = buffer.writer();

    writer.print("{s}({s}): ", .{ @tagName(level), @tagName(scope) }) catch unreachable;
    writer.print(fmt ++ "\n", args) catch unreachable;
    writer.writeByte(0) catch unreachable;

    const string = std.mem.span(@ptrCast([*:0]const u8, &bytes));

    if (term_req.response) |term_res| {
        const old_cr3 = asm volatile ("mov %%cr3, %[cr3]"
            : [cr3] "=r" (-> u64),
        );

        if (virt.kernel_address_space) |address_space| {
            if (old_cr3 != address_space.cr3) {
                asm volatile ("mov %[cr3], %%cr3"
                    :
                    : [cr3] "r" (address_space.cr3),
                );
            }

            term_res.write_fn(term_res.terminals[0], string, string.len);

            if (old_cr3 != address_space.cr3) {
                asm volatile ("mov %[cr3], %%cr3"
                    :
                    : [cr3] "r" (old_cr3),
                );
            }
        }
    }

    for (string) |byte| {
        arch.out(u8, 0xE9, byte);
    }
}
