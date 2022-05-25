#!/usr/bin/env sh

image_path=$1

qemu-system-x86_64 -cdrom $1 -debugcon stdio -smp 1 -m 1G -M q35 \
    -cpu qemu64,+smap,+smep -no-reboot -no-shutdown -s ${@:2}
