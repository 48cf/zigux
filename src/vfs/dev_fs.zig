const logger = std.log.scoped(.devfs);

const root = @import("root");
const std = @import("std");

const vfs = @import("../vfs.zig");
const per_cpu = @import("../per_cpu.zig");
const ps2 = @import("../drivers/ps2.zig");
const ram_fs = @import("ram_fs.zig");

const tty_vtable: vfs.VNodeVTable = .{
    .open = null,
    .read = TtyVNode.read,
    .write = TtyVNode.write,
    .insert = null,
    .mmap = null,
};

const TtyVNode = struct {
    vnode: vfs.VNode,

    fn read(vnode: *vfs.VNode, buffer: []u8, offset: usize) vfs.ReadError!usize {
        _ = vnode;
        _ = offset;

        while (true) {
            const event = ps2.getKeyboardEvent();

            if (!event.pressed) {
                continue;
            }

            const shift = ps2.keyboard_state.isShiftPressed();
            const ascii: u8 = switch (event.location) {
                .Number1 => if (shift) @as(u8, '!') else '1',
                .Number2 => if (shift) @as(u8, '@') else '2',
                .Number3 => if (shift) @as(u8, '#') else '3',
                .Number4 => if (shift) @as(u8, '$') else '4',
                .Number5 => if (shift) @as(u8, '%') else '5',
                .Number6 => if (shift) @as(u8, '^') else '6',
                .Number7 => if (shift) @as(u8, '&') else '7',
                .Number8 => if (shift) @as(u8, '*') else '8',
                .Number9 => if (shift) @as(u8, '(') else '9',
                .Number0 => if (shift) @as(u8, ')') else '0',
                .RightOf0 => if (shift) @as(u8, '_') else '-',
                .LeftOfBackspace => if (shift) @as(u8, '+') else '=',
                .Line1n1 => if (shift) @as(u8, 'Q') else 'q',
                .Line1n2 => if (shift) @as(u8, 'W') else 'w',
                .Line1n3 => if (shift) @as(u8, 'E') else 'e',
                .Line1n4 => if (shift) @as(u8, 'R') else 'r',
                .Line1n5 => if (shift) @as(u8, 'T') else 't',
                .Line1n6 => if (shift) @as(u8, 'Y') else 'y',
                .Line1n7 => if (shift) @as(u8, 'U') else 'u',
                .Line1n8 => if (shift) @as(u8, 'I') else 'i',
                .Line1n9 => if (shift) @as(u8, 'O') else 'o',
                .Line1n10 => if (shift) @as(u8, 'P') else 'p',
                .Line1n11 => if (shift) @as(u8, '{') else '[',
                .Line1n12 => if (shift) @as(u8, '}') else ']',
                .Line1n13 => if (shift) @as(u8, '|') else '\\',
                .Line2n1 => if (shift) @as(u8, 'A') else 'a',
                .Line2n2 => if (shift) @as(u8, 'S') else 's',
                .Line2n3 => if (shift) @as(u8, 'D') else 'd',
                .Line2n4 => if (shift) @as(u8, 'F') else 'f',
                .Line2n5 => if (shift) @as(u8, 'G') else 'g',
                .Line2n6 => if (shift) @as(u8, 'H') else 'h',
                .Line2n7 => if (shift) @as(u8, 'J') else 'j',
                .Line2n8 => if (shift) @as(u8, 'K') else 'k',
                .Line2n9 => if (shift) @as(u8, 'L') else 'l',
                .Line2n10 => if (shift) @as(u8, ':') else ';',
                .Line2n11 => if (shift) @as(u8, '"') else '\'',
                .Enter => return 0,
                .Line3n1 => if (shift) @as(u8, 'Z') else 'z',
                .Line3n2 => if (shift) @as(u8, 'X') else 'x',
                .Line3n3 => if (shift) @as(u8, 'C') else 'c',
                .Line3n4 => if (shift) @as(u8, 'V') else 'v',
                .Line3n5 => if (shift) @as(u8, 'B') else 'b',
                .Line3n6 => if (shift) @as(u8, 'N') else 'n',
                .Line3n7 => if (shift) @as(u8, 'M') else 'm',
                .Line3n8 => if (shift) @as(u8, ',') else '<',
                .Line3n9 => if (shift) @as(u8, '.') else '>',
                .Line3n10 => if (shift) @as(u8, '/') else '?',
                .Spacebar => @as(u8, ' '),
                else => continue,
            };

            logger.debug("{} => {c}", .{ event, ascii });

            buffer[0] = ascii;
            return 1;
        }
    }

    fn write(vnode: *vfs.VNode, buffer: []const u8, offset: usize) vfs.WriteError!usize {
        _ = vnode;
        _ = offset;

        const line = std.mem.split(u8, buffer, "\n").next().?;

        logger.info("{s}", .{line});

        return line.len;
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
