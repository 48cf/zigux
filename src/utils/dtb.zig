const logger = std.log.scoped(.dtb);

const std = @import("std");
const endian = @import("endian.zig");

pub const Header = extern struct {
    magic: endian.Big(u32),
    total_size: endian.Big(u32),
    off_dt_struct: endian.Big(u32),
    off_dt_strings: endian.Big(u32),
    off_mem_rsvmap: endian.Big(u32),
    version: endian.Big(u32),
    last_comp_version: endian.Big(u32),
    boot_cpuid_phys: endian.Big(u32),
    size_dt_strings: endian.Big(u32),
    size_dt_struct: endian.Big(u32),

    pub fn find(self: *const @This(), node_prefix: []const u8, prop_name: []const u8) ?[]const u8 {
        const pointer = @ptrToInt(self);

        var current = @intToPtr([*]endian.Big(u32), pointer + self.off_dt_struct.read());
        var current_depth: usize = 0;
        var found_at_depth: ?usize = null;

        while (true) {
            const opcode = current[0].read();
            current += 1;

            switch (opcode) {
                0x00000001 => { // FDT_BEGIN_NODE
                    const name = std.mem.span(@ptrCast([*:0]u8, current));

                    current_depth += 1;

                    if (found_at_depth == null and std.mem.startsWith(u8, name, node_prefix)) {
                        found_at_depth = current_depth;
                    }

                    current += (name.len + 4) / 4;
                },
                0x00000002 => { // FDT_END_NODE
                    if (found_at_depth) |depth| {
                        if (depth == current_depth) {
                            found_at_depth = null;
                        }
                    }

                    current_depth -= 1;
                },
                0x00000003 => { // FDT_PROP
                    const length = current[0].read();
                    const name_offset = current[1].read();
                    const name = std.mem.span(@intToPtr([*:0]u8, pointer + self.off_dt_strings.read() + name_offset));

                    if (found_at_depth) |depth| {
                        if (depth == current_depth and std.mem.eql(u8, name, prop_name)) {
                            return @ptrCast([*]u8, current + 2)[0..length];
                        }
                    }

                    current += (length + 3) / 4 + 2;
                },
                0x00000004 => {}, // FDT_NOP
                0x00000009 => break, // FDT_END
                else => std.debug.panic("Unhandled DTB opcode: {X:0>8}", .{opcode}),
            }
        }

        return null;
    }
};

pub fn parse(bytes: [*]align(@alignOf(Header)) const u8) !*const Header {
    const header = @ptrCast(*const Header, bytes);

    if (header.magic.read() != 0xD00DFEED) {
        return error.InvalidHeader;
    }

    return header;
}
