const logger = std.log.scoped(.debug);

const root = @import("root");
const std = @import("std");
const builtin = @import("builtin");

const arch = @import("arch.zig");
const virt = @import("virt.zig");

var debug_allocator_bytes: [16 * 1024 * 1024]u8 = undefined;
var debug_allocator = std.heap.FixedBufferAllocator.init(debug_allocator_bytes[0..]);
var debug_info: ?std.dwarf.DwarfInfo = null;

pub fn printStackIterator(stack_iter: std.debug.StackIterator) void {
    var iter = stack_iter;

    init() catch |nested_err| {
        logger.err("Failed to initialize debug info: {any}", .{nested_err});
    };

    logger.err("Stack backtrace:", .{});

    while (iter.next()) |addr| {
        printSymbol(addr);
    }
}

pub fn printStackTrace(stack_trace: *std.builtin.StackTrace) void {
    init() catch |nested_err| {
        logger.err("Failed to initialize debug info: {any}", .{nested_err});
    };

    logger.err("Stack backtrace:", .{});

    var frame_index: usize = 0;
    var frames_left: usize = @min(stack_trace.index, stack_trace.instruction_addresses.len);

    while (frames_left != 0) : ({
        frames_left -= 1;
        frame_index = (frame_index + 1) % stack_trace.instruction_addresses.len;
    }) {
        const return_address = stack_trace.instruction_addresses[frame_index];

        printSymbol(return_address);
    }
}

pub fn print(string: []const u8) void {
    const previous_vm = virt.kernel_address_space.switchTo();

    if (root.term_req.response) |term_res| {
        term_res.write(null, @as([*:0]const u8, @ptrCast(string))[0..string.len]);
    }

    _ = previous_vm.switchTo();
}

pub fn debugPrint(string: []const u8) void {
    for (string) |byte| {
        arch.out(u8, 0xE9, byte);
    }
}

fn init() !void {
    if (debug_info != null) {
        return;
    }

    errdefer debug_info = null;

    if (root.kernel_file_req.response) |kernel_file_res| {
        const kernel_file = kernel_file_res.kernel_file;

        var sections = std.dwarf.DwarfInfo.null_section_array;
        sections[@intFromEnum(std.dwarf.DwarfSection.debug_info)] = try getSectionSlice(kernel_file.address, ".debug_info");
        sections[@intFromEnum(std.dwarf.DwarfSection.debug_abbrev)] = try getSectionSlice(kernel_file.address, ".debug_abbrev");
        sections[@intFromEnum(std.dwarf.DwarfSection.debug_str)] = try getSectionSlice(kernel_file.address, ".debug_str");
        sections[@intFromEnum(std.dwarf.DwarfSection.debug_line)] = try getSectionSlice(kernel_file.address, ".debug_line");
        sections[@intFromEnum(std.dwarf.DwarfSection.debug_ranges)] = try getSectionSlice(kernel_file.address, ".debug_ranges");

        debug_info = .{
            .endian = .little,
            .is_macho = false,
            .sections = sections,
        };

        try std.dwarf.openDwarfDebugInfo(&debug_info.?, debug_allocator.allocator());
    } else {
        return error.NoKernelFile;
    }
}

fn printInfo(address: u64, symbol_name: []const u8, file_name_: []const u8, line: usize) void {
    var file_name = file_name_;

    if (std.mem.startsWith(u8, file_name, "/base_dir")) {
        while (!std.mem.startsWith(u8, file_name, "kernel/")) {
            const idx = std.mem.indexOf(u8, file_name, "/") orelse break;
            file_name = file_name[idx + 1 ..];
        }
    }

    logger.err("  0x{X:0>16}: {s} at {s}:{d}", .{ address, symbol_name, file_name, line });
}

fn printSymbol(address: u64) void {
    var symbol_name: []const u8 = "<no symbol info>";

    if (debug_info) |*info| brk: {
        if (info.getSymbolName(address)) |name| {
            symbol_name = name;
        }

        const compile_unit = info.findCompileUnit(address) catch break :brk;
        const line_info = info.getLineNumberInfo(debug_allocator.allocator(), compile_unit.*, address) catch break :brk;

        return printInfo(address, symbol_name, line_info.file_name, line_info.line);
    }

    printInfo(address, symbol_name, "??", 0);
}

fn getSectionData(elf: [*]const u8, shdr: []const u8) []const u8 {
    const offset = @as(usize, @intCast(std.mem.readInt(u64, shdr[24..][0..8], .little)));
    const size = @as(usize, @intCast(std.mem.readInt(u64, shdr[32..][0..8], .little)));

    return elf[offset .. offset + size];
}

fn getSectionName(names: []const u8, shdr: []const u8) ?[]const u8 {
    const offset = @as(usize, @intCast(std.mem.readInt(u32, shdr[0..][0..4], .little)));
    const len = std.mem.indexOf(u8, names[offset..], "\x00") orelse return null;

    return names[offset .. offset + len];
}

fn getShdr(elf: [*]const u8, idx: u16) []const u8 {
    const sh_offset = std.mem.readInt(u64, elf[40 .. 40 + 8], .little);
    const sh_entsize = std.mem.readInt(u16, elf[58 .. 58 + 2], .little);
    const off = sh_offset + sh_entsize * @as(usize, @intCast(idx));

    return elf[off .. off + sh_entsize];
}

fn getSectionSlice(elf: [*]const u8, section_name: []const u8) !std.dwarf.DwarfInfo.Section {
    const sh_strndx = std.mem.readInt(u16, elf[62 .. 62 + 2], .little);
    const sh_num = std.mem.readInt(u16, elf[60 .. 60 + 2], .little);

    if (sh_strndx > sh_num) {
        return error.ShstrndxOutOfRange;
    }

    const section_names = getSectionData(elf, getShdr(elf, sh_strndx));

    var i: u16 = 0;

    while (i < sh_num) : (i += 1) {
        const header = getShdr(elf, i);

        if (std.mem.eql(u8, getSectionName(section_names, header) orelse continue, section_name)) {
            const data = getSectionData(elf, header);
            return .{ .data = data, .owned = false };
        }
    }

    return error.SectionNotFound;
}
