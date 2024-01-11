// https://github.com/FlorenceOS/Florence/blob/master/lib/format/tar.zig

const std = @import("std");

pub const FileType = enum(u8) {
    Normal = '0',
    HardLink = '1',
    SymbolicLink = '2',
    CharacterDevice = '3',
    BlockDevice = '4',
    Directory = '5',
    Fifo = '6',
    ContiguousFile = '7',
    LongFileName = 'L',
    _,
};

pub const File = struct {
    name: []const u8,
    kind: FileType,
    data: []const u8,
    link: []const u8,
};

const TarHeader = extern struct {
    file_name: [100]u8,
    file_mode: [8]u8,
    uid: [8]u8,
    gid: [8]u8,
    file_size: [12]u8,
    last_modified: [12]u8,
    header_checksum: [8]u8,
    link_indicator: u8,
    linked_name: [100]u8,
    padding: [255]u8,

    fn validate(self: *const TarHeader) bool {
        for (self.file_size[0..11]) |ch| {
            if (ch < '0' or ch > '9') {
                return false;
            }
        }

        if (self.file_size[11] != 0) {
            return false;
        }

        return true;
    }

    fn fileName(self: *const TarHeader) []const u8 {
        for (self.file_name, 0..) |ch, i| {
            if (ch == 0) {
                return self.file_name[0..i];
            }
        }

        return self.file_name[0..self.file_name.len];
    }

    fn linkedName(self: *const TarHeader) []const u8 {
        for (self.linked_name, 0..) |ch, i| {
            if (ch == 0) {
                return self.linked_name[0..i];
            }
        }

        return self.linked_name[0..self.linked_name.len];
    }

    fn fileSize(self: *const TarHeader) !usize {
        return std.fmt.parseUnsigned(usize, self.file_size[0..11], 8);
    }
};

const TarIterator = struct {
    buffer: []const u8,
    name_override: ?[]const u8,
    offset: usize,

    pub fn next(self: *TarIterator) !?File {
        const header_buf = self.buffer[self.offset..];

        if (header_buf.len < @sizeOf(TarHeader)) {
            return error.UnexpectedEof;
        }

        // We expect at least two null entries at the end of the file
        if (std.mem.allEqual(u8, header_buf[0..@sizeOf(TarHeader)], 0)) {
            const next_header_buf = self.buffer[self.offset + @sizeOf(TarHeader) ..];

            if (next_header_buf.len < @sizeOf(TarHeader)) {
                return error.UnexpectedEof;
            }

            if (!std.mem.allEqual(u8, next_header_buf[0..@sizeOf(TarHeader)], 0)) {
                return error.FormatError;
            }

            return null;
        }

        const header = @as(*const TarHeader, @ptrCast(header_buf));

        if (!header.validate()) {
            return error.InvalidHeader;
        }

        const file_size = try header.fileSize();
        const block_leftover = file_size % 512;
        const file_block_size = if (block_leftover == 0) file_size else file_size + 512 - block_leftover;

        self.offset += @sizeOf(TarHeader) + file_block_size;

        // TODO: Handle files with long names properly
        if (header.link_indicator == 'L') {
            self.name_override = header_buf[@sizeOf(TarHeader) .. @sizeOf(TarHeader) + file_size - 1];
            return self.next();
        }

        const name = self.name_override orelse header.fileName();

        self.name_override = null;

        return .{
            .name = name,
            .kind = @as(FileType, @enumFromInt(header.link_indicator)),
            .data = header_buf[@sizeOf(TarHeader) .. @sizeOf(TarHeader) + file_size],
            .link = header.linkedName(),
        };
    }
};

pub fn iterate(bytes: []const u8) TarIterator {
    return .{
        .buffer = bytes,
        .name_override = null,
        .offset = 0,
    };
}
