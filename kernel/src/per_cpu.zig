const root = @import("root");
const std = @import("std");

const arch = @import("arch.zig");
const phys = @import("phys.zig");
const virt = @import("virt.zig");
const scheduler = @import("scheduler.zig");
const process = @import("process.zig");

pub const PerCpu = struct {
    self: *PerCpu,
    gdt: arch.Gdt = .{},
    tss: arch.Tss = .{},
    idt: arch.Idt = .{},
    lapic_base: u64 = 0,
    thread: ?*scheduler.Thread = null,

    pub fn currentProcess(self: *PerCpu) ?*process.Process {
        if (self.thread) |thread| {
            return thread.parent;
        } else {
            return null;
        }
    }
};

var bsp_percpu: PerCpu = .{ .self = undefined };

pub fn initBsp() void {
    bsp_percpu = .{
        .self = &bsp_percpu,
        .lapic_base = virt.asHigherHalf(u64, arch.Msr.apic.read() & ~@as(u64, 0xFFF)),
    };

    arch.Msr.gs_base.write(@intFromPtr(&bsp_percpu));
    arch.Msr.gs_kernel_base.write(0);
}

pub fn initFeatures() void {
    var cr4 = asm volatile ("mov %%cr4, %[result]"
        : [result] "=r" (-> u64),
    );

    cr4 |= 1 << 9;
    cr4 |= 1 << 16;

    asm volatile ("mov %[value], %%cr4"
        :
        : [value] "r" (cr4),
    );
}

pub fn init() !void {
    var instance = try root.allocator.create(PerCpu);

    instance.* = .{
        .self = instance,
        .lapic_base = virt.asHigherHalf(u64, arch.Msr.apic.read() & ~@as(u64, 0xFFF)),
    };

    const intr_stack = root.page_heap_allocator.allocate(4) orelse return error.OutOfMemory;
    const ist_stack = root.page_heap_allocator.allocate(4) orelse return error.OutOfMemory;
    const sched_stack = root.page_heap_allocator.allocate(4) orelse return error.OutOfMemory;
    const pf_stack = root.page_heap_allocator.allocate(4) orelse return error.OutOfMemory;

    instance.tss.rsp[0] = intr_stack + std.mem.page_size * 4;
    instance.tss.ist[0] = ist_stack + std.mem.page_size * 4;
    instance.tss.ist[1] = sched_stack + std.mem.page_size * 4;
    instance.tss.ist[2] = pf_stack + std.mem.page_size * 4;

    instance.gdt.load(&instance.tss);
    instance.idt.load();

    arch.Msr.gs_base.write(@intFromPtr(instance));
    arch.Msr.gs_kernel_base.write(0);
}

pub inline fn get() *PerCpu {
    return asm volatile ("mov %%gs:0, %[result]"
        : [result] "=r" (-> *PerCpu),
    );
}
