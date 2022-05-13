inline fn limine_magic(magic_a: u64, magic_b: u64) [4]u64 {
    return .{ 0xc7b1dd30df4c8b88, 0x0a82e883a194f07b, magic_a, magic_b };
}

pub const Uuid = extern struct {
    a: u32,
    b: u16,
    c: u16,
    d: [8]u8,
};

pub const File = extern struct {
    pub const MediaType = enum(u32) {
        Generic,
        Optical,
        Tftp,
    };

    revision: u64,
    address: [*]const u8,
    size: u64,
    path: [*:0]const u8,
    cmdline: [*:0]const u8,
    media_type: MediaType,
    unused: u32,
    tftp_ip: u32,
    tftp_port: u32,
    partition_index: u32,
    mbr_disk_id: u32,
    gpt_disk_uuid: Uuid,
    gpt_partition_uuid: Uuid,
    partition_uuid: Uuid,
};

pub const BootloaderInfo = extern struct {
    pub const Response = extern struct {
        revision: u64,
        name: [*:0]const u8,
        version: [*:0]const u8,
    };

    pub const Request = extern struct {
        id: [4]u64 = limine_magic(0xf55038d8e2a1202f, 0x279426fcf5f59740),
        revision: u64,
        response: ?*Response = null,
    };
};

pub const StackSize = extern struct {
    pub const Response = extern struct {
        revision: u64,
    };

    pub const Request = extern struct {
        id: [4]u64 = limine_magic(0x224ef0460a8e8926, 0xe1cb0fc25f46ea3d),
        revision: u64,
        response: ?*Response = null,
        stack_size: u64,
    };
};

pub const Hhdm = extern struct {
    pub const Response = extern struct {
        revision: u64,
        offset: u64,
    };

    pub const Request = extern struct {
        id: [4]u64 = limine_magic(0x48dcf1cb8ad2b852, 0x63984e959a98244b),
        revision: u64,
        response: ?*Response = null,
    };
};

pub const Framebuffer = extern struct {
    pub const Info = extern struct {
        address: [*]u8,
        width: u16,
        height: u16,
        pitch: u16,
        bpp: u16,
        memory_model: u8,
        red_mask_size: u8,
        red_mask_shift: u8,
        green_mask_size: u8,
        green_mask_shift: u8,
        blue_mask_size: u8,
        blue_mask_shift: u8,
        unused: u8,
        edid_size: u64,
        edid: [*]const u8,
    };

    pub const Response = extern struct {
        revision: u64,
        framebuffer_count: u64,
        framebuffers: [*]*Info,
    };

    pub const Request = extern struct {
        id: [4]u64 = limine_magic(0xcbfe81d7dd2d1977, 0x063150319ebc9b71),
        revision: u64,
        response: ?*Response = null,
    };
};

pub const Terminal = extern struct {
    pub const CallbackType = enum(u64) {
        Dec = 10,
        Bell = 20,
        PrivateId = 30,
        StatusReport = 40,
        PosReport = 50,
        KbdLeds = 60,
        Mode = 70,
        Linux = 80,
    };

    pub const Command = enum(u64) {
        CtxSize = -1,
        CtxSave = -2,
        CtxRestore = -3,
        FullRefresh = -4,
    };

    pub const Info = extern struct {
        columns: u32,
        rows: u32,
        framebuffer: ?*Framebuffer.Info,
    };

    pub const Response = extern struct {
        revision: u64,
        terminal_count: u64,
        terminals: [*]*Info,
        write_fn: fn (*Info, [*:0]const u8, u64) callconv(.C) void,
    };

    pub const Request = extern struct {
        id: [4]u64 = limine_magic(0x0785a0aea5d0750f, 0x1c1936fee0d6cf6e),
        revision: u64,
        response: ?*Response = null,
        callback: ?fn (*Info, u64, u64, u64, u64) callconv(.C) void = null,
    };
};

pub const FiveLevelPaging = extern struct {
    pub const Response = extern struct {
        revision: u64,
    };

    pub const Request = extern struct {
        id: [4]u64 = limine_magic(0x94469551da9b3192, 0xebe5e86db7382888),
        revision: u64,
        response: ?*Response = null,
    };
};

pub const Smp = extern struct {
    pub const Flags = enum(u32) {
        X2Apic = 1 << 0,
    };

    pub const Info = extern struct {
        processor_id: u32,
        lapic_id: u32,
        reserved: u64,
        goto_address: fn (*Info) callconv(.C) void,
        extra_argument: u64,
    };

    pub const Response = extern struct {
        revision: u64,
        flags: Flags,
        bsp_lapic_id: u32,
        cpu_count: u64,
        cpus: [*]*Info,
    };

    pub const Request = extern struct {
        id: [4]u64 = limine_magic(0x95a67b819a1b857e, 0xa0b61b723b6a73e0),
        revision: u64,
        response: ?*Response = null,
        flags: Flags = @intToEnum(Flags, 0),
        unused: u32 = 0,
    };
};

pub const MemoryMap = extern struct {
    pub const Entry = extern struct {
        pub const Type = enum(u64) {
            Usable,
            Reserved,
            AcpiReclaimable,
            AcpiNvs,
            BadMemory,
            BootloaderReclaimable,
            KernelAndModules,
            Framebuffer,
        };

        base: u64,
        length: u64,
        kind: Type,
    };

    pub const Response = extern struct {
        revision: u64,
        entry_count: u64,
        entries: [*]*Entry,
    };

    pub const Request = extern struct {
        id: [4]u64 = limine_magic(0x67cf3d9d378a806f, 0xe304acdfc50c3c62),
        revision: u64,
        response: ?*Response = null,
    };
};

pub const EntryPoint = extern struct {
    pub const Response = extern struct {
        revision: u64,
    };

    pub const Request = extern struct {
        id: [4]u64 = limine_magic(0x13d86c035a1cd3e1, 0x2b0caa89d8f3026a),
        revision: u64,
        response: ?*Response = null,
        entry: fn () callconv(.C) void,
    };
};

pub const KernelFile = extern struct {
    pub const Response = extern struct {
        revision: u64,
        kernel_file: *File,
    };

    pub const Request = extern struct {
        id: [4]u64 = limine_magic(0xad97e90e83f1ed67, 0x31eb5d1c5ff23b69),
        revision: u64,
        response: ?*Response = null,
    };
};

pub const Modules = extern struct {
    pub const Response = extern struct {
        revision: u64,
        module_count: u64,
        modules: [*]*File,
    };

    pub const Request = extern struct {
        id: [4]u64 = limine_magic(0x3e7e279702be32af, 0xca1c4f3bd1280cee),
        revision: u64,
        response: ?*Response = null,
    };
};

pub const Rsdp = extern struct {
    pub const Response = extern struct {
        revision: u64,
        address: u64,
    };

    pub const Request = extern struct {
        id: [4]u64 = limine_magic(0xc5e77b6b397e7b43, 0x27637845accdcf3c),
        revision: u64,
        response: ?*Response = null,
    };
};

pub const Smbios = extern struct {
    pub const Response = extern struct {
        revision: u64,
        entry_32: u64,
        entry_64: u64,
    };

    pub const Request = extern struct {
        id: [4]u64 = limine_magic(0x9e9046f11e095391, 0xaa4a520fefbde5ee),
        revision: u64,
        response: ?*Response = null,
    };
};

pub const EfiSystemTable = extern struct {
    pub const Response = extern struct {
        revision: u64,
        address: u64,
    };

    pub const Request = extern struct {
        id: [4]u64 = limine_magic(0x5ceba5163eaaf6d6, 0x0a6981610cf65fcc),
        revision: u64,
        response: ?*Response = null,
    };
};

pub const BootTime = extern struct {
    pub const Response = extern struct {
        revision: u64,
        boot_time: i64,
    };

    pub const Request = extern struct {
        id: [4]u64 = limine_magic(0x502746e184c088aa, 0xfbc5ec83e6327893),
        revision: u64,
        response: ?*Response = null,
    };
};

pub const KernelAddress = extern struct {
    pub const Response = extern struct {
        revision: u64,
        physical_base: u64,
        virtual_base: u64,
    };

    pub const Request = extern struct {
        id: [4]u64 = limine_magic(0x71ba76863cc55f63, 0xb2644a48c516a487),
        revision: u64,
        response: ?*Response = null,
    };
};
