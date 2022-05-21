#!/usr/bin/env sh

image_dir=$1
image_path=$2
sysroot_dir=$3
sysroot_path=$4

make -C limine >/dev/null 2>&1
mkdir -p $image_dir >/dev/null 2>&1
tar -C $sysroot_dir -cf $sysroot_path . >/dev/null 2>&1
cp ${@:5} $sysroot_path misc/limine.cfg limine/limine.sys limine/limine-cd.bin $image_dir >/dev/null 2>&1
xorriso -as mkisofs -b limine-cd.bin -no-emul-boot -boot-info-table --protective-msdos-label $image_dir -o $image_path >/dev/null 2>&1
limine/limine-deploy $image_path >/dev/null 2>&1
