const std = @import("std");

fn kernel_target(comptime arch: std.Target.Cpu.Arch) std.zig.CrossTarget {
    var target: std.zig.CrossTarget = .{};
    target.cpu_arch = arch;
    target.os_tag = .freestanding;
    target.abi = .none;

    switch (arch) {
        .x86_64 => {
            const Feature = std.Target.x86.Feature;
            target.cpu_features_add.addFeature(@intFromEnum(Feature.soft_float));
            target.cpu_features_sub.addFeature(@intFromEnum(Feature.mmx));
            target.cpu_features_sub.addFeature(@intFromEnum(Feature.sse));
            target.cpu_features_sub.addFeature(@intFromEnum(Feature.sse2));
            target.cpu_features_sub.addFeature(@intFromEnum(Feature.avx));
            target.cpu_features_sub.addFeature(@intFromEnum(Feature.avx2));
        },
        else => @panic("Unsupported CPU architecture: " ++ @tagName(arch)),
    }

    return target;
}

pub fn build(b: *std.Build) !void {
    const kernel_arch = .x86_64;

    const optimize = b.standardOptimizeOption(.{});
    const kernel = b.addExecutable(.{
        .name = "kernel",
        .root_source_file = .{ .path = "src/main.zig" },
        .target = kernel_target(kernel_arch),
        .optimize = optimize,
    });

    kernel.code_model = .large;
    kernel.want_lto = false;

    kernel.addAnonymousModule("limine", .{ .source_file = .{ .path = "limine-zig/limine.zig" } });
    kernel.addCSourceFile(.{ .file = .{ .path = "flanterm/flanterm.c" }, .flags = &.{} });
    kernel.addCSourceFile(.{ .file = .{ .path = "flanterm/backends/fb.c" }, .flags = &.{} });

    kernel.addIncludePath(.{ .path = "flanterm" });
    kernel.setLinkerScriptPath(.{ .path = "linker." ++ @tagName(kernel_arch) ++ ".ld" });

    const kernel_install = b.addInstallArtifact(kernel, .{});

    b.default_step.dependOn(&kernel_install.step);
}
