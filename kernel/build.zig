const std = @import("std");

const target = blk: {
    var tgt = std.zig.CrossTarget{
        .cpu_arch = .x86_64,
        .os_tag = .freestanding,
        .abi = .none,
    };

    const Features = std.Target.x86.Feature;

    tgt.cpu_features_sub.addFeature(@enumToInt(Features.mmx));
    tgt.cpu_features_sub.addFeature(@enumToInt(Features.sse));
    tgt.cpu_features_sub.addFeature(@enumToInt(Features.sse2));
    tgt.cpu_features_sub.addFeature(@enumToInt(Features.avx));
    tgt.cpu_features_sub.addFeature(@enumToInt(Features.avx2));
    tgt.cpu_features_add.addFeature(@enumToInt(Features.soft_float));

    break :blk tgt;
};

pub fn build(b: *std.build.Builder) !void {
    const kernel = b.addExecutable(.{
        .name = "kernel",
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = .Debug,
    });

    kernel.code_model = .large;

    kernel.setLinkerScriptPath(.{ .path = "linker.ld" });
    kernel.addAnonymousModule("limine", .{ .source_file = .{ .path = "limine-zig/limine.zig" } });
    kernel.addIncludePath("../pkgs/mlibc-headers/usr/include");
    kernel.addIncludePath("../pkgs/linux-headers/usr/include");

    const kernel_install = b.addInstallArtifact(kernel);

    b.default_step.dependOn(&kernel_install.step);
}