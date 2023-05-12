pub const C = @cImport({
    @cInclude("abi-bits/errno.h");
    @cInclude("abi-bits/seek-whence.h");
    @cInclude("abi-bits/stat.h");
    @cInclude("abi-bits/vm-flags.h");
    @cInclude("asm/ioctls.h");
    @cInclude("zigux/syscall.h");
    @cInclude("dirent.h");
    @cInclude("fcntl.h");
    @cInclude("termios.h");
});
