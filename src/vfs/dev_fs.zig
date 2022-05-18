const logger = std.log.scoped(.devfs);

const root = @import("root");
const std = @import("std");

const vfs = @import("../vfs.zig");
const ps2 = @import("../drivers/ps2.zig");
const ram_fs = @import("ram_fs.zig");

const tty_vtable: vfs.VNodeVTable = .{
    .open = null,
    .read = TtyVNode.read,
    .write = null,
    .insert = null,
};

const TtyVNode = struct {
    vnode: vfs.VNode,

    fn read(vnode: *vfs.VNode, buffer: []u8, offset: usize) vfs.ReadWriteError!usize {
        _ = vnode;
        _ = buffer;
        _ = offset;

        const event = ps2.getKeyboardEvent();

        logger.debug("Processing keyboard event: {}", .{event});

        return 0;
    }
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
