const builtin = @import("builtin");
const this_arch = switch (builtin.cpu.arch) {
    .x86_64 => @import("arch/x86_64.zig"),
    else => |arch| @compileError("Unsupported architecture: " ++ @tagName(arch)),
};

pub const Cpu = extern struct {
    this_ptr: *Cpu,
    arch_cpu: this_arch.ArchCpu,
};

pub usingnamespace this_arch;
