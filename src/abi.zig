usingnamespace @cImport({
    @cInclude("abis/mlibc/errno.h");
    @cInclude("abis/mlibc/stat.h");
    @cInclude("options/posix/include/dirent.h");
    @cInclude("options/posix/include/termios.h");
});

pub const O_ACCMODE = 0x0007;
pub const O_EXEC = 1;
pub const O_RDONLY = 2;
pub const O_RDWR = 3;
pub const O_SEARCH = 4;
pub const O_WRONLY = 5;
pub const O_APPEND = 0x00008;
pub const O_CREAT = 0x00010;
pub const O_DIRECTORY = 0x00020;
pub const O_EXCL = 0x00040;
pub const O_NOCTTY = 0x00080;
pub const O_NOFOLLOW = 0x00100;
pub const O_TRUNC = 0x00200;
pub const O_NONBLOCK = 0x00400;
pub const O_DSYNC = 0x00800;
pub const O_RSYNC = 0x01000;
pub const O_SYNC = 0x02000;
pub const O_CLOEXEC = 0x04000;
pub const O_PATH = 0x08000;
pub const O_LARGEFILE = 0x10000;
pub const O_NOATIME = 0x20000;
pub const O_ASYNC = 0x40000;

pub const PROT_NONE = 0x00;
pub const PROT_READ = 0x01;
pub const PROT_WRITE = 0x02;
pub const PROT_EXEC = 0x04;

pub const MAP_FILE = 0x00;
pub const MAP_PRIVATE = 0x01;
pub const MAP_SHARED = 0x02;
pub const MAP_FIXED = 0x04;
pub const MAP_ANON = 0x08;
pub const MAP_ANONYMOUS = 0x08;
pub const MAP_NORESERVE = 0x10;

pub const SEEK_CUR = 1;
pub const SEEK_END = 2;
pub const SEEK_SET = 3;

pub const ARCH_SET_GS = 0x1001;
pub const ARCH_SET_FS = 0x1002;
pub const ARCH_GET_FS = 0x1003;
pub const ARCH_GET_GS = 0x1004;
