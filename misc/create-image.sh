#!/usr/bin/env sh

image_dir=$1
image_path=$2
sysroot_dir=$3
sysroot_path=$4

if [ ! -d "$sysroot_dir" ]; then
  jinx sysroot
fi

make -C limine >/dev/null 2>&1
mkdir -p "$image_dir" >/dev/null 2>&1

if [ ! -f "$sysroot_path" ] || [ "$sysroot_dir" -nt "$sysroot_path" ]; then
  tar -C "$sysroot_dir" -cf "$sysroot_path" . >/dev/null 2>&1
  touch -r "$sysroot_dir" "$sysroot_path"
fi

sysroot_in_image="$image_dir"/"$(basename $sysroot_path)"

if [ ! -f "$sysroot_in_image" ] || [ "$sysroot_path" -nt "$sysroot_in_image" ]; then
  cp "$sysroot_path" "$sysroot_in_image"
fi

cp ${@:5} misc/limine.cfg limine/limine.sys limine/limine-cd.bin "$image_dir" >/dev/null 2>&1
xorriso -as mkisofs -b limine-cd.bin -no-emul-boot -boot-info-table --protective-msdos-label "$image_dir" -o "$image_path" >/dev/null 2>&1
limine/limine-deploy "$image_path" >/dev/null 2>&1
