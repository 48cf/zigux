pub const Msr = enum(u32) {
    apic = 0x1b,
    ia32_efer = 0xc0000080,
    star = 0xc0000081,
    lstar = 0xc0000082,
    cstar = 0xc0000083,
    sfmask = 0xc0000084,
    fs_base = 0xc0000100,
    gs_base = 0xc0000101,
    kernel_gs_base = 0xc0000102,
};

pub inline fn write(msr: Msr, value: u64) void {
    asm volatile ("wrmsr"
        :
        : [_] "{eax}" (value & 0xffffffff),
          [_] "{edx}" ((value >> 32) & 0xffffffff),
          [_] "{ecx}" (@intFromEnum(msr)),
    );
}

pub inline fn read(msr: Msr) u64 {
    var low: u32 = undefined;
    var high: u32 = undefined;
    asm volatile ("rdmsr"
        : [_] "={eax}" (low),
          [_] "={edx}" (high),
        : [_] "{ecx}" (@intFromEnum(msr)),
    );

    return @as(u64, low) | @as(u64, high) << 32;
}
