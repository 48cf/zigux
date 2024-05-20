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
    // Get dependencies
    const flanterm = b.dependency("flanterm", .{});
    const limine = b.dependency("limine", .{});
    const uacpi = b.dependency("uacpi", .{});

    // Build the kernel
    const optimize = b.standardOptimizeOption(.{});
    const kernel = b.addExecutable(.{
        .name = "zigux",
        .root_source_file = .{ .path = "src/main.zig" },
        .target = b.resolveTargetQuery(target),
        .optimize = optimize,
        .code_model = .kernel,
        .omit_frame_pointer = false,
        .pic = true,
    });

    kernel.setLinkerScriptPath(.{ .path = "linker.ld" });
    kernel.root_module.addImport("limine", limine.module("limine"));
    kernel.root_module.red_zone = false;
    kernel.root_module.stack_check = false;
    kernel.want_lto = false;

    // Install the kernel as an artifact
    b.installArtifact(kernel);

    // Add mlibc include paths
    kernel.addIncludePath(.{ .path = "../pkgs/mlibc-headers/usr/include" });
    kernel.addIncludePath(.{ .path = "../pkgs/linux-headers/usr/include" });

    // Get dependency paths
    const flanterm_path = flanterm.builder.build_root.path.?;
    const uacpi_path = uacpi.builder.build_root.path.?;

    // Add flanterm include path and source files
    kernel.addIncludePath(.{ .path = flanterm_path });
    kernel.addCSourceFiles(.{
        .root = .{ .path = flanterm_path },
        .files = &.{ "flanterm.c", "backends/fb.c" },
        .flags = &.{"-fno-sanitize=undefined"},
    });

    // Add uACPI include path and source files
    kernel.addIncludePath(.{
        .path = try std.fs.path.join(b.allocator, &.{ uacpi_path, "include" }),
    });

    kernel.addCSourceFiles(.{
        .root = .{ .path = uacpi_path },
        .files = &.{
            "source/tables.c",
            "source/types.c",
            "source/uacpi.c",
            "source/utilities.c",
            "source/interpreter.c",
            "source/opcodes.c",
            "source/namespace.c",
            "source/stdlib.c",
            "source/shareable.c",
            "source/opregion.c",
            "source/default_handlers.c",
            "source/io.c",
            "source/notify.c",
            "source/sleep.c",
            "source/registers.c",
            "source/resources.c",
            "source/event.c",
        },
    });

    // Enable sized frees in uACPI
    kernel.defineCMacro("UACPI_SIZED_FREES", "1");
}
