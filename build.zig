const std = @import("std");
const builtin = @import("builtin");

const Features = std.Target.x86.Feature;

const kernel_build_mode: std.builtin.Mode = .Debug;
const user_build_mode: std.builtin.Mode = .ReleaseSafe;

const target = blk: {
    var tgt = std.zig.CrossTarget{
        .cpu_arch = .x86_64,
        .os_tag = .freestanding,
        .abi = .none,
    };

    tgt.cpu_features_sub.addFeature(@enumToInt(Features.mmx));
    tgt.cpu_features_sub.addFeature(@enumToInt(Features.sse));
    tgt.cpu_features_sub.addFeature(@enumToInt(Features.sse2));
    tgt.cpu_features_sub.addFeature(@enumToInt(Features.avx));
    tgt.cpu_features_sub.addFeature(@enumToInt(Features.avx2));
    tgt.cpu_features_add.addFeature(@enumToInt(Features.soft_float));

    break :blk tgt;
};

pub fn build(b: *std.build.Builder) !void {
    // const init = try buildProgram(b, "init");
    // const init_path = b.getInstallPath(init.install_step.?.dest_dir, init.out_filename);
    //
    // image.step.dependOn(&init.install_step.?.step);

    const kernel = try buildKernel(b);

    const image_dir = b.pathJoin(&.{ b.cache_root, "image_root" });
    const image_path = b.pathJoin(&.{ b.cache_root, "image.iso" });
    const sysroot_path = b.pathJoin(&.{ b.cache_root, "sysroot.tar" });

    const kernel_path = b.getInstallPath(kernel.install_step.?.dest_dir, kernel.out_filename);

    const qemu = b.addSystemCommand(&.{ "/usr/bin/env", "sh", "misc/run-emulator.sh", image_path });
    const image = b.addSystemCommand(&.{
        "/usr/bin/env",
        "sh",
        "misc/create-image.sh",
        image_dir,
        image_path,
        "sysroot",
        sysroot_path,
        kernel_path,
    });

    image.step.dependOn(&kernel.install_step.?.step);
    qemu.step.dependOn(&image.step);

    if (b.args) |args| {
        qemu.addArgs(args);
    }

    b.step("image", "Builds the image").dependOn(&image.step);
    b.step("run", "Runs the image").dependOn(&qemu.step);
}

fn buildKernel(b: *std.build.Builder) !*std.build.LibExeObjStep {
    const kernel = b.addExecutable("kernel", "src/main.zig");

    kernel.code_model = .kernel;
    kernel.setBuildMode(kernel_build_mode);
    kernel.setLinkerScriptPath(.{ .path = "misc/linker.ld" });
    kernel.addIncludeDir("sources/mlibc");
    kernel.addIncludeDir("sources/mlibc/sysdeps/zigux/include");
    kernel.install();
    kernel.setTarget(target);

    return kernel;
}

fn buildProgram(b: *std.build.Builder, comptime name: []const u8) !*std.build.LibExeObjStep {
    const kernel = b.addExecutable(name, "user/" ++ name ++ "/main.zig");

    kernel.code_model = .small;
    kernel.setBuildMode(user_build_mode);
    kernel.install();
    kernel.setTarget(target);

    return kernel;
}
