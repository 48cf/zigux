Zigux is an attempt to write a UNIX-like kernel in Zig.
In order to build Zigux you need an x86_64 based Linux system and following dependencies:

- GNU coreutils
- GNU make
- wget
- xorriso
- qemu

Running `make all` will generate an ISO file in the current directory. You can emulate
Zigux by running `make run` (or `make run-kvm` if your host supports KVM).

Building packages and kernel itself is handled by Jinx (https://github.com/mintsuki/jinx),
the recipes were greatly inspired by Lyre (https://github.com/lyre-os/lyre).
