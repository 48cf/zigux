const root = @import("root");
const std = @import("std");

const apic = @import("./apic.zig");
const arch = @import("./arch.zig");
const phys = @import("./phys.zig");
const process = @import("./process.zig");
const scheduler = @import("./scheduler.zig");
const utils = @import("./utils.zig");
const virt = @import("./virt.zig");

pub const PerCpu = struct {
    self: *PerCpu,
    gdt: arch.GDT = .{},
    idt: arch.IDT = .{},
    tss: arch.TSS = std.mem.zeroes(arch.TSS),
    lapic_base: u64 = 0,
    lapic_id: u32 = 0,
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
        .lapic_base = virt.asHigherHalf(u64, arch.rdmsr(.IA32_APIC_BASE) & ~@as(u64, 0xFFF)),
    };

    arch.wrmsr(.IA32_GS_BASE, @intFromPtr(&bsp_percpu));
    arch.wrmsr(.IA32_KERNEL_GS_BASE, 0);

    bsp_percpu.gdt.load();
    bsp_percpu.idt.load(false);
}

pub fn initFeatures() void {
    var cr4 = asm volatile ("mov %%cr4, %[result]"
        : [result] "=r" (-> u64),
    );

    cr4 |= (1 << 9);

    asm volatile ("mov %[value], %%cr4"
        :
        : [value] "r" (cr4),
    );
}

pub fn init() !void {
    const instance = try root.allocator.create(PerCpu);
    instance.* = .{
        .self = instance,
        .lapic_base = virt.asHigherHalf(u64, arch.rdmsr(.IA32_APIC_BASE) & ~@as(u64, 0xFFF)),
    };

    const intr_stack = try utils.KernelStack.allocate(4);
    const ist_stack = try utils.KernelStack.allocate(4);
    const sched_stack = try utils.KernelStack.allocate(4);
    const pf_stack = try utils.KernelStack.allocate(4);

    instance.lapic_id = apic.getLapicID();
    instance.tss.rsp[0] = intr_stack.getEndAddress();
    instance.tss.ist[0] = ist_stack.getEndAddress();
    instance.tss.ist[1] = sched_stack.getEndAddress();
    instance.tss.ist[2] = pf_stack.getEndAddress();

    instance.gdt.load();
    instance.gdt.loadTSS(&instance.tss);
    instance.idt.load(true);

    arch.wrmsr(.IA32_GS_BASE, @intFromPtr(&bsp_percpu));
    arch.wrmsr(.IA32_KERNEL_GS_BASE, 0);
}

pub inline fn get() *PerCpu {
    return asm volatile ("mov %%gs:0, %[result]"
        : [result] "=r" (-> *PerCpu),
    );
}
