// https://github.com/FlorenceOS/Florence/blob/master/lib/format/tar.zig

const std = @import("std");

const abi = @import("./abi.zig");

pub const FileType = enum(u8) {
    normal = '0',
    hard_link = '1',
    symbolic_link = '2',
    character_device = '3',
    block_device = '4',
    directory = '5',
    fifo = '6',
    contiguous_file = '7',
    long_file_name = 'L',
    _,
};

pub const File = struct {
    name: []const u8,
    kind: FileType,
    data: []const u8,
    link: []const u8,
    uid: abi.uid_t,
    gid: abi.gid_t,
    mtime: abi.time_t,
    mode: abi.mode_t,
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

    fn validate(self: *const @This()) bool {
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

    fn fileName(self: *const @This()) []const u8 {
        const length = std.mem.indexOfScalar(u8, &self.file_name, 0) orelse self.file_name.len;
        return self.file_name[0..length];
    }

    fn fileMode(self: *const @This()) !abi.mode_t {
        return std.fmt.parseUnsigned(abi.mode_t, self.file_mode[0..7], 8);
    }

    fn getUID(self: *const @This()) !abi.uid_t {
        return std.fmt.parseUnsigned(abi.uid_t, self.uid[0..7], 8);
    }

    fn getGID(self: *const @This()) !abi.gid_t {
        return std.fmt.parseUnsigned(abi.gid_t, self.gid[0..7], 8);
    }

    fn lastModified(self: *const @This()) !abi.time_t {
        return std.fmt.parseUnsigned(abi.time_t, self.last_modified[0..11], 8);
    }

    fn linkedName(self: *const @This()) []const u8 {
        const length = std.mem.indexOfScalar(u8, &self.linked_name, 0) orelse self.linked_name.len;
        return self.linked_name[0..length];
    }

    fn fileSize(self: *const @This()) !usize {
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
            .uid = try header.getUID(),
            .gid = try header.getGID(),
            .mtime = try header.lastModified(),
            .mode = try header.fileMode(),
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
