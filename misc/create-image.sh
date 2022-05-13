#!/bin/sh

kernel_path=$1
image_dir=$2
image_path=$3

make -C limine >/dev/null 2>&1
mkdir -p $image_dir >/dev/null 2>&1
cp $kernel_path misc/limine.cfg limine/limine.sys limine/limine-cd.bin $image_dir >/dev/null 2>&1
xorriso -as mkisofs -b limine-cd.bin -no-emul-boot -boot-info-table --protective-msdos-label $image_dir -o $image_path >/dev/null 2>&1
limine/limine-deploy $image_path >/dev/null 2>&1
