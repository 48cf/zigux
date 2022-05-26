const logger = std.log.scoped(.interrupts);

const std = @import("std");

const arch = @import("arch.zig");
const debug = @import("debug.zig");
const per_cpu = @import("per_cpu.zig");
const virt = @import("virt.zig");

var next_vector: usize = 32;
var handlers = [1]InterruptHandler{exceptionHandler} ** 32 ++ [1]InterruptHandler{unhandledInterruptHandler} ** 224;

pub const syscall_vector: u8 = 0xFD;
pub const sched_call_vector: u8 = 0xFE;
pub const spurious_vector: u8 = 0xFF;

pub const InterruptStub = fn () callconv(.Naked) void;
pub const InterruptHandler = fn (*InterruptFrame) void;

pub const InterruptFrame = extern struct {
    es: u64,
    ds: u64,
    r15: u64,
    r14: u64,
    r13: u64,
    r12: u64,
    r11: u64,
    r10: u64,
    r9: u64,
    r8: u64,
    rsi: u64,
    rdi: u64,
    rbp: u64,
    rdx: u64,
    rcx: u64,
    rbx: u64,
    rax: u64,
    vector: u64,
    error_code: u64,
    rip: u64,
    cs: u64,
    rflags: u64,
    rsp: u64,
    ss: u64,
};

pub fn makeHandlers() [256]fn () callconv(.Naked) void {
    var result = [1]InterruptStub{undefined} ** 256;

    comptime var i: usize = 0;

    inline while (i < 256) : (i += 1) {
        result[i] = comptime makeHandler(i);
    }

    return result;
}

pub fn allocateVector() u8 {
    var result = @atomicRmw(usize, &next_vector, .Add, 1, .AcqRel);

    if (result >= 256 - 16) {
        @panic("No more interrupt vectors left to allocate");
    }

    return @truncate(u8, result);
}

pub fn registerHandler(vector: u8, handler: InterruptHandler) void {
    handlers[vector] = handler;
}

fn printRegisters(frame: *InterruptFrame) void {
    const cr2 = asm volatile ("mov %%cr2, %[result]"
        : [result] "=r" (-> u64),
    );

    const cr3 = asm volatile ("mov %%cr3, %[result]"
        : [result] "=r" (-> u64),
    );

    if (per_cpu.get().thread) |thread| {
        logger.err("Faulting process: {}:{}", .{ thread.parent.pid, thread.tid });
    }

    logger.err("Registers:", .{});
    logger.err("  RAX={X:0>16} RBX={X:0>16} RCX={X:0>16} RDX={X:0>16}", .{ frame.rax, frame.rbx, frame.rcx, frame.rdx });
    logger.err("  RSI={X:0>16} RDI={X:0>16} RBP={X:0>16} RSP={X:0>16}", .{ frame.rsi, frame.rdi, frame.rbp, frame.rsp });
    logger.err("   R8={X:0>16}  R9={X:0>16} R10={X:0>16} R11={X:0>16}", .{ frame.r8, frame.r9, frame.r10, frame.r11 });
    logger.err("  R12={X:0>16} R13={X:0>16} R14={X:0>16} R15={X:0>16}", .{ frame.r12, frame.r13, frame.r14, frame.r15 });
    logger.err("  RIP={X:0>16} CR2={X:0>16} CR3={X:0>16} ERR={X:0>16}", .{ frame.rip, cr2, cr3, frame.error_code });
}

fn exceptionHandler(frame: *InterruptFrame) void {
    const cpu_info = per_cpu.get();

    if (frame.vector == 0x6) {
        const code = @intToPtr([*]const u8, frame.rip);

        if (std.mem.eql(u8, code[0..2], &.{ 0x0f, 0x05 })) {
            frame.rip += 2;

            return @call(.{ .modifier = .always_tail }, handlers[syscall_vector], .{frame});
        }
    } else if (frame.vector == 0xE) blk: {
        const cr2 = asm volatile ("mov %%cr2, %[result]"
            : [result] "=r" (-> u64),
        );

        const handled = virt.handlePageFault(cr2, frame.error_code) catch |err| {
            logger.err("Failed to handle the page fault: {e}", .{err});

            break :blk;
        };

        if (handled) {
            return;
        }
    }

    logger.err("An exception #{} occurred", .{frame.vector});

    debug.printStackIterator(std.debug.StackIterator.init(@returnAddress(), @frameAddress()));

    printRegisters(frame);

    if (cpu_info.currentProcess()) |process| {
        // TODO: Implement signals and stuff like that so we can properly
        // terminate processes that violate memory protections

        logger.info("Killed {}:{} because of a protection violation", .{ process.pid, cpu_info.thread.?.tid });

        process.exit_code = 0xff;

        cpu_info.thread = null;
    } else {
        while (true) {
            arch.halt();
        }
    }
}

fn unhandledInterruptHandler(frame: *InterruptFrame) void {
    logger.err("An unhandled interrupt #{} occurred", .{frame.vector});

    debug.printStackIterator(std.debug.StackIterator.init(@returnAddress(), @frameAddress()));

    printRegisters(frame);

    while (true) {
        arch.halt();
    }
}

fn makeHandler(comptime vector: usize) InterruptStub {
    return struct {
        fn handler() callconv(.Naked) void {
            // https://wiki.osdev.org/Exceptions
            const has_error_code = switch (vector) {
                0x8 => true,
                0xA...0xE => true,
                0x11 => true,
                0x15 => true,
                0x1D...0x1E => true,
                else => false,
            };

            if (comptime (has_error_code)) {
                asm volatile (
                    \\pushq %[vector]
                    \\jmp interruptCommonHandler
                    :
                    : [vector] "i" (vector),
                );
            } else {
                asm volatile (
                    \\pushq $0
                    \\pushq %[vector]
                    \\jmp interruptCommonHandler
                    :
                    : [vector] "i" (vector),
                );
            }
        }
    }.handler;
}

export fn interruptHandler(frame: *InterruptFrame) callconv(.C) void {
    const handler = handlers[frame.vector & 0xFF];

    handler(frame);
}

export fn swapGsIfNeeded(frame: *InterruptFrame) callconv(.C) void {
    if (frame.cs != 0x28) {
        asm volatile ("swapgs");
    }
}

export fn interruptCommonHandler() callconv(.Naked) void {
    asm volatile (
        \\push %%rax
        \\push %%rbx
        \\push %%rcx
        \\push %%rdx
        \\push %%rbp
        \\push %%rdi
        \\push %%rsi
        \\push %%r8
        \\push %%r9
        \\push %%r10
        \\push %%r11
        \\push %%r12
        \\push %%r13
        \\push %%r14
        \\push %%r15
        \\xor %%rax, %%rax
        \\mov %%ds, %%ax
        \\push %%rax
        \\mov %%es, %%ax
        \\push %%rax
        \\
        \\mov %%rsp, %%rdi
        \\call swapGsIfNeeded
        \\mov %%rsp, %%rdi
        \\call interruptHandler
        \\mov %%rsp, %%rdi
        \\call swapGsIfNeeded
        \\
        \\pop %%rax
        \\mov %%ax, %%es
        \\pop %%rax
        \\mov %%ax, %%ds
        \\pop %%r15
        \\pop %%r14
        \\pop %%r13
        \\pop %%r12
        \\pop %%r11
        \\pop %%r10
        \\pop %%r9
        \\pop %%r8
        \\pop %%rsi
        \\pop %%rdi
        \\pop %%rbp
        \\pop %%rdx
        \\pop %%rcx
        \\pop %%rbx
        \\pop %%rax
        \\
        \\add $16, %%rsp
        \\iretq
    );
}
