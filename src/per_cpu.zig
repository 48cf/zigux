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
    const instance = try root.allocator.create(PerCpu);

    instance.* = .{
        .self = instance,
        .lapic_base = virt.asHigherHalf(u64, arch.Msr.apic.read() & ~@as(u64, 0xFFF)),
    };

    const intr_stack = phys.allocate(1, true) orelse return error.OutOfMemory;
    const ist_stack = phys.allocate(1, true) orelse return error.OutOfMemory;

    instance.tss.rsp[0] = virt.asHigherHalf(u64, intr_stack + std.mem.page_size);
    instance.tss.ist[0] = virt.asHigherHalf(u64, ist_stack + std.mem.page_size);

    instance.gdt.load(&instance.tss);
    instance.idt.load();

    arch.Msr.gs_base.write(@ptrToInt(instance));
    arch.Msr.gs_kernel_base.write(0);
}

pub inline fn get() *PerCpu {
    return tryGet().?;
}

pub inline fn tryGet() ?*PerCpu {
    return asm volatile ("rdgsbase %[result]"
        : [result] "=r" (-> *PerCpu),
    );
}
