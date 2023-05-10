pub const C = @cImport({
    @cInclude("abis/mlibc/errno.h");
    @cInclude("abis/mlibc/seek-whence.h");
    @cInclude("abis/mlibc/stat.h");
    @cInclude("abis/mlibc/vm-flags.h");
    @cInclude("asm/ioctls.h");
    @cInclude("zigux/syscall.h");
    @cInclude("dirent.h");
    @cInclude("fcntl.h");
    @cInclude("termios.h");
});
