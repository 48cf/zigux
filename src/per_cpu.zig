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
    lapic_base: u64,
    thread: ?*scheduler.Thread = null,

    pub fn currentProcess(self: *PerCpu) ?*process.Process {
        if (self.thread) |thread| {
            return thread.parent;
        } else {
            return null;
        }
    }
};

pub fn init() !void {
    const instance = try root.allocator.create(PerCpu);
    const intr_stack = phys.allocate(1, true) orelse return error.OutOfMemory;

    instance.* = .{
        .self = instance,
        .lapic_base = virt.hhdm + arch.Msr.apic.read() & ~@as(u64, 0xFFF),
    };

    instance.tss.rsp[0] = virt.hhdm + intr_stack + std.mem.page_size;

    instance.gdt.load(&instance.tss);
    instance.idt.load();

    arch.Msr.gs_base.write(@ptrToInt(instance));
    arch.Msr.gs_kernel_base.write(0);
}

pub inline fn get() *PerCpu {
    return asm volatile ("mov %%gs:[0], %[result]"
        : [result] "=r" (-> *PerCpu),
    );
}
