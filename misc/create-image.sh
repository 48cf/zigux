#!/bin/sh

set -ex

# Prepare the sysroot.
rm -rf sysroot
./jinx sysroot
./jinx host-build limine

# Make an initramfs tarball from the sysroot.
(cd sysroot && tar cf ../initramfs.tar *)

# Prepare the ISO root.
rm -rf iso_root
mkdir -p iso_root
cp pkgs/kernel/usr/bin/kernel iso_root/
cp initramfs.tar iso_root/
cp misc/limine.cfg iso_root/
cp host-pkgs/limine/usr/local/share/limine/limine-bios.sys iso_root/
cp host-pkgs/limine/usr/local/share/limine/limine-bios-cd.bin iso_root/

# Create the ISO.
xorriso -as mkisofs -b limine-bios-cd.bin \
  -no-emul-boot -boot-load-size 4 -boot-info-table \
  --protective-msdos-label iso_root -o zigux.iso

# Install Limine.
host-pkgs/limine/usr/local/bin/limine bios-install zigux.iso
