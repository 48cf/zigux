#!/bin/bash

set -ex

# Prepare the sysroot.
rm -rf sysroot
./jinx sysroot
./jinx host-build limine

limine_dir="host-pkgs/limine/usr/local/"

# Prepare the ISO root.
rm -rf iso_root
mkdir -p iso_root
mkdir -p iso_root/EFI/BOOT
cp pkgs/kernel/usr/bin/kernel iso_root/
cp $limine_dir/share/limine/limine-bios.sys iso_root/
cp $limine_dir/share/limine/limine-bios-cd.bin iso_root/
cp $limine_dir/share/limine/limine-uefi-cd.bin iso_root/
cp $limine_dir/share/limine/BOOTX64.EFI iso_root/EFI/BOOT/

# Make an initramfs tarball from the sysroot.
if type pigz >/dev/null 2>&1; then
    tar -I pigz -C sysroot -cf iso_root/initramfs.tar.gz .
else
    tar -C sysroot -czf iso_root/initramfs.tar.gz .
fi

# Write the limine config
cat <<EOF >iso_root/limine.cfg
TIMEOUT=0
SERIAL=yes

:kernel
    PROTOCOL=limine
    KERNEL_PATH=boot:///kernel
    MODULE_PATH=\$boot:///initramfs.tar.gz
EOF

# Create the ISO.
xorriso -as mkisofs -b limine-bios-cd.bin \
    -no-emul-boot -boot-load-size 4 -boot-info-table \
    --efi-boot limine-uefi-cd.bin -efi-boot-part --efi-boot-image \
    --protective-msdos-label iso_root -o zigux.iso

# Install Limine.
$limine_dir/bin/limine bios-install zigux.iso
