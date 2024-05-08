#!/bin/bash

set -ex

# Prepare the sysroot.
rm -rf sysroot
./jinx sysroot
./jinx host-build limine

# Make an initramfs tarball from the sysroot.
(cd sysroot && tar cf ../initramfs.tar *)

limine_dir="host-pkgs/limine/usr/local/"

# Prepare the ISO root.
rm -rf iso_root
mkdir -p iso_root
mkdir -p iso_root/EFI/BOOT
cp pkgs/kernel/usr/bin/kernel iso_root/
cp initramfs.tar iso_root/
cp misc/limine.cfg iso_root/
cp $limine_dir/share/limine/limine-bios.sys iso_root/
cp $limine_dir/share/limine/limine-bios-cd.bin iso_root/
cp $limine_dir/share/limine/limine-uefi-cd.bin iso_root/
cp $limine_dir/share/limine/BOOTX64.EFI iso_root/EFI/BOOT/

# Create the ISO.
xorriso -as mkisofs -b limine-bios-cd.bin \
    -no-emul-boot -boot-load-size 4 -boot-info-table \
    --efi-boot limine-uefi-cd.bin -efi-boot-part --efi-boot-image \
    --protective-msdos-label iso_root -o zigux.iso

# Install Limine.
$limine_dir/bin/limine bios-install zigux.iso
