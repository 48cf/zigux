#!/usr/bin/env sh

image_dir=$1
image_path=$2

make -C limine >/dev/null 2>&1
mkdir -p $image_dir >/dev/null 2>&1
cp ${@:3} misc/limine.cfg limine/limine.sys limine/limine-cd.bin $image_dir >/dev/null 2>&1
xorriso -as mkisofs -b limine-cd.bin -no-emul-boot -boot-info-table --protective-msdos-label $image_dir -o $image_path >/dev/null 2>&1
limine/limine-deploy $image_path >/dev/null 2>&1
