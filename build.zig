const std = @import("std");
const builtin = @import("builtin");

const Features = std.Target.x86.Feature;

pub fn build(b: *std.build.Builder) !void {
    const kernel = b.addExecutable("kernel", "src/main.zig");

    var target = std.zig.CrossTarget{
        .cpu_arch = .x86_64,
        .os_tag = .freestanding,
        .abi = .none,
    };

    target.cpu_features_sub.addFeature(@enumToInt(Features.mmx));
    target.cpu_features_sub.addFeature(@enumToInt(Features.sse));
    target.cpu_features_sub.addFeature(@enumToInt(Features.sse2));
    target.cpu_features_sub.addFeature(@enumToInt(Features.avx));
    target.cpu_features_sub.addFeature(@enumToInt(Features.avx2));
    target.cpu_features_add.addFeature(@enumToInt(Features.soft_float));

    kernel.code_model = .kernel;
    kernel.setBuildMode(.Debug);
    kernel.setLinkerScriptPath(.{ .path = "misc/linker.ld" });
    kernel.install();
    kernel.setTarget(target);

    const image_dir = b.pathJoin(&.{ b.cache_root, "image_root" });
    const image_path = b.pathJoin(&.{ b.cache_root, "image.iso" });
    const kernel_path = b.getInstallPath(kernel.install_step.?.dest_dir, kernel.out_filename);

    const image = b.addSystemCommand(&.{ "/bin/sh", "misc/create-image.sh", kernel_path, image_dir, image_path });
    const qemu = b.addSystemCommand(&.{ "/bin/sh", "misc/run-emulator.sh", image_path });

    image.step.dependOn(&kernel.install_step.?.step);
    qemu.step.dependOn(&image.step);

    b.step("image", "Builds the image").dependOn(&image.step);
    b.step("run", "Runs the image").dependOn(&qemu.step);
}
