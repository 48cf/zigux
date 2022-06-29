const std = @import("std");

const Builder = std.build.Builder;
const CrossTarget = std.zig.CrossTarget;
const Features = std.Target.aarch64.Feature;

const target = blk: {
    var tgt = CrossTarget{
        .cpu_arch = .aarch64,
        .os_tag = .freestanding,
        .abi = .none,
    };

    tgt.cpu_features_sub.addFeature(@enumToInt(Features.fp_armv8));
    tgt.cpu_features_sub.addFeature(@enumToInt(Features.crypto));
    tgt.cpu_features_sub.addFeature(@enumToInt(Features.neon));

    break :blk tgt;
};

pub fn build(b: *Builder) !void {
    const kernel = b.addExecutable("kernel", "src/main.zig");

    kernel.code_model = .large;
    kernel.setBuildMode(.ReleaseSafe);
    kernel.setLinkerScriptPath(.{ .path = "src/linker.ld" });
    kernel.install();
    kernel.setTarget(target);

    const kernel_blob = kernel.installRaw(b.fmt("{s}.bin", .{kernel.out_filename}), .{
        .format = .bin,
        .only_section_name = ".blob",
        .pad_to_size = 64 * 1024 * 1024, // 64M
    });

    const kernel_blob_path = b.getInstallPath(kernel_blob.dest_dir, kernel_blob.dest_filename);
    const command_step = b.step("qemu-virt", "Run built kernel in QEMU");
    const run_step = b.addSystemCommand(&[_][]const u8{
        // zig fmt: off
        "qemu-system-aarch64",
        "-machine", "virt", "-cpu", "cortex-a57",
        "-serial", "stdio", "-m", "2G", "-smp", "8",
        "-drive", b.fmt("if=pflash,format=raw,file={s},readonly=on", .{kernel_blob_path}),
        // zig fmt: on
    });

    run_step.step.dependOn(&kernel_blob.step);
    command_step.dependOn(&run_step.step);
}
