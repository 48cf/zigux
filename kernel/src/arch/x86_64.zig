const arch = @import("../arch.zig");
const msr = @import("x86_64/msr.zig");
const port = @import("x86_64/port.zig");

const DescriptorTableRegister = extern struct {
    limit: u16 align(1),
    base: u64 align(1),
};

const GdtEntry = extern struct {
    limit: u16,
    base_low: u16,
    base_mid: u8,
    flags: u8,
    granularity: u8,
    base_high: u8,
};

const GdtEntryExtended = extern struct {
    lower: GdtEntry,
    base_high_ex: u32,
    reserved: u32,
};

const Tss = extern struct {
    reserved: u32 align(1) = 0,
    rsp: [3]u64 align(1) = .{ 0, 0, 0 },
    reserved0: u64 align(1) = 0,
    ist: [7]u64 align(1) = .{ 0, 0, 0, 0, 0, 0, 0 },
    reserved1: u32 align(1) = 0,
    reserved2: u32 align(1) = 0,
    reserved3: u16 align(1) = 0,
    iopb_offset: u16 align(1) = 0,
};

pub const ArchCpu = extern struct {
    lapic_id: u32,
    lapic_address: u64,
    gdt: [7]u64 = .{
        0x0000000000000000, // null
        0x00af9b000000ffff, // 64-bit code
        0x00af93000000ffff, // 64-bit data
        0x00aff3000000ffff, // usermode 64-bit data
        0x00affb000000ffff, // usermode 64-bit code
        0x0000000000000000, // tss low
        0x0000000000000000, // tss high
    },
    tss: Tss,
};

const CpuIdResult = struct {
    eax: u32,
    ebx: u32,
    ecx: u32,
    edx: u32,
};

inline fn cpuid(leaf: u32, subleaf: u32) CpuIdResult {
    var eax: u32 = undefined;
    var ebx: u32 = undefined;
    var ecx: u32 = undefined;
    var edx: u32 = undefined;

    asm volatile ("cpuid"
        : [_] "={eax}" (eax),
          [_] "={ebx}" (ebx),
          [_] "={ecx}" (ecx),
          [_] "={edx}" (edx),
        : [_] "{eax}" (leaf),
          [_] "{ecx}" (subleaf),
    );

    return .{ .eax = eax, .ebx = ebx, .ecx = ecx, .edx = edx };
}

pub fn init_cpu(cpu: *arch.Cpu) void {
    // Initialize the this_ptr just to be extra sure
    cpu.this_ptr = cpu;

    // Initialize the TSS entry in the GDT
    const tss_entry = @as(*GdtEntryExtended, @ptrCast(&cpu.arch_cpu.gdt[5]));
    const tss_addr = @intFromPtr(&cpu.arch_cpu.tss);
    tss_entry.* = .{
        .lower = .{
            .limit = @sizeOf(Tss) - 1,
            .base_low = @as(u16, @truncate(tss_addr)),
            .base_mid = @as(u8, @truncate(tss_addr >> 16)),
            .flags = 0b10001001,
            .granularity = 0,
            .base_high = @as(u8, @truncate(tss_addr >> 24)),
        },
        .base_high_ex = @as(u32, @truncate(tss_addr >> 32)),
        .reserved = 0,
    };

    const gdtr: DescriptorTableRegister = .{
        .limit = @sizeOf(u64) * 7 - 1,
        .base = @intFromPtr(&cpu.arch_cpu.gdt),
    };

    // Load the GDT
    asm volatile (
        \\lgdt (%[gdtr])
        \\ltr %[tss_sel]
        :
        : [gdtr] "r" (&gdtr),
          [tss_sel] "r" (@as(u16, 0x28)),
    );

    // Check if the processor supports SYSCALL/SYSRET instructions
    const cpuid_result = cpuid(0x80000001, undefined);
    if ((cpuid_result.edx & 1 << 11) == 0) {
        @panic("Cannot proceed without SYSCALL/SYSRET support");
    }

    // Set IA32_EFER.SCE
    const ia32_efer = msr.read(.ia32_efer);
    msr.write(.ia32_efer, ia32_efer | 1 << 0);

    // Set up syscall support
    msr.write(.star, 0);
    msr.write(.lstar, 0);
    msr.write(.cstar, 0); // No compatibility mode syscall handler
    msr.write(.sfmask, 0x200); // Disable interrupts on syscall entry

    // Set GS and kernel GS base registers
    set_current_cpu(cpu);
}

pub inline fn current_cpu() *arch.Cpu {
    return asm volatile ("mov %%gs:(0x0), %[out]"
        : [out] "=r" (-> *arch.Cpu),
    );
}

pub inline fn set_current_cpu(cpu: *arch.Cpu) void {
    msr.write(.gs_base, @intFromPtr(cpu));
    msr.write(.kernel_gs_base, @intFromPtr(cpu));
}

pub inline fn debug_print(buffer: []const u8) void {
    for (buffer) |byte| {
        port.out(u8, 0xe9, byte);
    }
}
