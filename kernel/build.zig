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

    kernel.root_module.red_zone = false;
    kernel.root_module.stack_check = false;
    kernel.root_module.omit_frame_pointer = false;
    kernel.want_lto = false;

    kernel.root_module.addImport("limine", limine.module("limine"));

    kernel.setLinkerScriptPath(.{ .path = "linker.ld" });

    // mlibc includes
    kernel.addIncludePath(.{ .path = "../pkgs/mlibc-headers/usr/include" });
    kernel.addIncludePath(.{ .path = "../pkgs/linux-headers/usr/include" });

    const c_flags: []const []const u8 = &.{
        "-ffreestanding",
        "-fno-sanitize=all",
        "-march=x86-64",
        "-mno-80387",
        "-mno-mmx",
        "-mno-sse",
        "-mno-sse2",
        "-mno-red-zone",
    };

    // uacpi includes
    kernel.addIncludePath(.{ .path = "./uacpi/include" });

    // uacpi sources
    kernel.defineCMacro("UACPI_SIZED_FREES", "1");
    kernel.addCSourceFiles(.{
        .files = &.{
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
        },
        .flags = c_flags,
    });

    // printf includes
    kernel.addIncludePath(.{ .path = "./printf/src" });

    // printf sources
    kernel.defineCMacro("PRINTF_ALIAS_STANDARD_FUNCTION_NAMES", "1");
    kernel.defineCMacro("PRINTF_ALIAS_STANDARD_FUNCTION_NAMES_HARD", "1");
    kernel.defineCMacro("PRINTF_SUPPORT_DECIMAL_SPECIFIERS", "0");
    kernel.defineCMacro("PRINTF_SUPPORT_EXPONENTIAL_SPECIFIERS", "0");
    kernel.addCSourceFiles(.{
        .files = &.{"./printf/src/printf/printf.c"},
        .flags = c_flags,
    });

    // flanterm includes
    kernel.addIncludePath(.{ .path = "./flanterm" });

    // flanterm sources
    kernel.defineCMacro("FLANTERM_FB_DISABLE_BUMP_ALLOC", "1");
    kernel.addCSourceFiles(.{
        .files = &.{
            "./flanterm/flanterm.c",
            "./flanterm/backends/fb.c",
        },
        .flags = c_flags,
    });

    b.installArtifact(kernel);
}
