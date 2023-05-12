#!/usr/bin/env sh

qemu_args="-cdrom $1 -debugcon stdio -smp 1 -m 2G -M q35,smm=off -enable-kvm -cpu qemu64,+fsgsbase -no-reboot -no-shutdown -s -d int -D int_log"

qemu-system-x86_64 ${qemu_args} ${@:2}
