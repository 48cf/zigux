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
        .pic = true,
        .code_model = .kernel,
        .root_source_file = .{ .path = "src/main.zig" },
        .target = b.resolveTargetQuery(target),
        .optimize = optimize,
    });

    kernel.root_module.red_zone = false;
    kernel.root_module.stack_check = false;
    kernel.root_module.omit_frame_pointer = false;
    kernel.want_lto = false;

    kernel.root_module.addImport("limine", limine.module("limine"));

    kernel.setLinkerScriptPath(.{ .path = "linker.ld" });

    // implementation for some standard library functions
    kernel.addCSourceFile(.{ .file = .{ .path = "./src/runtime.c" } });

    // mlibc includes
    kernel.addIncludePath(.{ .path = "../pkgs/mlibc-headers/usr/include" });
    kernel.addIncludePath(.{ .path = "../pkgs/linux-headers/usr/include" });

    // uacpi includes
    kernel.addIncludePath(.{ .path = "./uacpi/include" });

    // uacpi sources
    kernel.defineCMacro("UACPI_SIZED_FREES", "1");
    kernel.addCSourceFiles(.{ .files = &.{
        "./uacpi/source/tables.c",
        "./uacpi/source/types.c",
        "./uacpi/source/uacpi.c",
        "./uacpi/source/utilities.c",
        "./uacpi/source/interpreter.c",
        "./uacpi/source/opcodes.c",
        "./uacpi/source/namespace.c",
        "./uacpi/source/stdlib.c",
        "./uacpi/source/shareable.c",
        "./uacpi/source/opregion.c",
        "./uacpi/source/default_handlers.c",
        "./uacpi/source/io.c",
        "./uacpi/source/notify.c",
        "./uacpi/source/sleep.c",
        "./uacpi/source/registers.c",
        "./uacpi/source/resources.c",
        "./uacpi/source/event.c",
    } });

    // printf includes
    kernel.addIncludePath(.{ .path = "./nanoprintf" });

    // flanterm includes
    kernel.addIncludePath(.{ .path = "./flanterm" });

    // flanterm sources
    kernel.addCSourceFiles(.{
        .files = &.{
            "./flanterm/flanterm.c",
            "./flanterm/backends/fb.c",
        },
        .flags = &.{"-fno-sanitize=undefined"},
    });

    b.installArtifact(kernel);
}
