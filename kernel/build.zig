const std = @import("std");

const target = blk: {
    var tgt = std.zig.CrossTarget{
        .cpu_arch = .x86_64,
        .os_tag = .freestanding,
        .abi = .none,
    };

    const Features = std.Target.x86.Feature;

    tgt.cpu_features_sub.addFeature(@intFromEnum(Features.mmx));
    tgt.cpu_features_sub.addFeature(@intFromEnum(Features.sse));
    tgt.cpu_features_sub.addFeature(@intFromEnum(Features.sse2));
    tgt.cpu_features_sub.addFeature(@intFromEnum(Features.avx));
    tgt.cpu_features_sub.addFeature(@intFromEnum(Features.avx2));
    tgt.cpu_features_add.addFeature(@intFromEnum(Features.soft_float));

    break :blk tgt;
};

pub fn build(b: *std.Build) !void {
    const optimize = b.standardOptimizeOption(.{});
    const limine = b.dependency("limine", .{});
    const kernel = b.addExecutable(.{
        .name = "kernel",
        .code_model = .kernel,
        .pic = true,
        .root_source_file = .{ .path = "src/main.zig" },
        .target = b.resolveTargetQuery(target),
        .optimize = optimize,
    });

    kernel.root_module.addImport("limine", limine.module("limine"));

    kernel.setLinkerScriptPath(.{ .path = "linker.ld" });
    kernel.addIncludePath(.{ .path = "../pkgs/mlibc-headers/usr/include" });
    kernel.addIncludePath(.{ .path = "../pkgs/linux-headers/usr/include" });

    b.installArtifact(kernel);
}
