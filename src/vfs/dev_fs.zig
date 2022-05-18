const logger = std.log.scoped(.devfs);

const root = @import("root");
const std = @import("std");

const vfs = @import("../vfs.zig");
const ram_fs = @import("ram_fs.zig");

const tty_vtable: vfs.VNodeVTable = .{
    .open = null,
    .read = null,
    .write = null,
    .insert = null,
};

const TtyVNode = struct {
    vnode: vfs.VNode,
};

pub fn init(name: []const u8, parent: ?*vfs.VNode) !*vfs.VNode {
    const ramfs = try ram_fs.init(name, parent);
    const tty = try root.allocator.create(TtyVNode);

    tty.* = .{
        .vnode = .{
            .vtable = &tty_vtable,
            .filesystem = ramfs.filesystem,
            .name = "tty",
        },
    };

    try ramfs.insert(&tty.vnode);

    return ramfs;
}
