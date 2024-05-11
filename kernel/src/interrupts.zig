const logger = std.log.scoped(.interrupts);

const std = @import("std");

const arch = @import("arch.zig");
const debug = @import("debug.zig");
const scheduler = @import("scheduler.zig");
const per_cpu = @import("per_cpu.zig");
const virt = @import("virt.zig");

var next_vector: usize = 32;
var handlers = [1]InterruptHandler{exceptionHandler} ** 32 ++ [1]InterruptHandler{unhandledInterruptHandler} ** 224;
var handler_contexts: [256]InterruptContext = undefined;

pub const syscall_vector: u8 = 0xFD;
pub const sched_call_vector: u8 = 0xFE;
pub const spurious_vector: u8 = 0xFF;

pub const InterruptContext = ?*anyopaque;

const InterruptStub = *const fn () callconv(.Naked) void;
const InterruptHandler = *const fn (*InterruptFrame) void;
const InterruptHandlerWithContext = *const fn (InterruptContext) void;

const IretFrame = extern struct {
    err: u64,
    rip: u64,
    cs: u64,
    rflags: u64,
    rsp: u64,
    ss: u64,
};

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
    iret: IretFrame,
};

const interrupt_stubs = blk: {
    var result: [256]InterruptStub = undefined;
    for (&result, 0..) |*it, i| {
        it.* = makeHandler(i);
    }
    break :blk result;
};

pub fn getInterruptHandlers() [256]InterruptStub {
    return interrupt_stubs;
}

pub fn allocateVector() u8 {
    const result = @atomicRmw(usize, &next_vector, .Add, 1, .acq_rel);
    if (result >= 256 - 16) {
        @panic("No more interrupt vectors left to allocate");
    }
    return @intCast(result);
}

pub fn registerHandler(vector: u8, handler: InterruptHandler) void {
    handlers[vector] = handler;
}

pub fn registerHandlerWithContext(
    vector: u8,
    comptime handler_: InterruptHandlerWithContext,
    context: InterruptContext,
) void {
    handler_contexts[vector] = context;
    handlers[vector] = struct {
        fn handler(frame: *InterruptFrame) void {
            const handler_context = handler_contexts[frame.vector & 0xFF];
            @call(.always_inline, handler_, .{handler_context});
        }
    }.handler;
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
    logger.err("  rax={X:0>16} rbx={X:0>16} rcx={X:0>16} rdx={X:0>16}", .{ frame.rax, frame.rbx, frame.rcx, frame.rdx });
    logger.err("  rsi={X:0>16} rdi={X:0>16} rbp={X:0>16} rsp={X:0>16}", .{ frame.rsi, frame.rdi, frame.rbp, frame.iret.rsp });
    logger.err("   r8={X:0>16}  r9={X:0>16} r10={X:0>16} r11={X:0>16}", .{ frame.r8, frame.r9, frame.r10, frame.r11 });
    logger.err("  r12={X:0>16} r13={X:0>16} r14={X:0>16} r15={X:0>16}", .{ frame.r12, frame.r13, frame.r14, frame.r15 });
    logger.err("  rip={X:0>16} cr2={X:0>16} cr3={X:0>16} err={X:0>16}", .{ frame.iret.rip, cr2, cr3, frame.iret.err });
}

fn exceptionHandler(frame: *InterruptFrame) void {
    const cpu_info = per_cpu.get();

    if (frame.vector == 0x6) {
        const code = @as([*]const u8, @ptrFromInt(frame.iret.rip));

        if (std.mem.eql(u8, code[0..2], &.{ 0x0F, 0x05 })) {
            frame.iret.rip += 2;
            return @call(.always_tail, handlers[syscall_vector], .{frame});
        }
    } else if (frame.vector == 0xE) blk: {
        const cr2 = asm volatile ("mov %%cr2, %[result]"
            : [result] "=r" (-> u64),
        );

        // If the access violation comes from kernel mode (CPL=0) and the
        // first bit of error code (present bit) is not set, then we can
        // make sure that we did in fact hit a guard page of a kernel stack
        if ((frame.iret.cs & 0x3) == 0 and (frame.iret.err & (1 << 0)) == 0) {
            if (virt.kernel_address_space.page_table.getPTE(cr2, false)) |pte| {
                if ((pte.getFlags() & virt.PTEFlags.guard_page) != 0) {
                    @panic("A kernel stack guard page was hit!!! O_O");
                }
            }
        }

        if (virt.handlePageFault(cr2, frame.iret.err) catch |err| {
            logger.err("Failed to handle the page fault: {any}", .{err});
            break :blk;
        }) {
            return;
        }
    }

    logger.err("An exception #{} occurred", .{frame.vector});
    debug.printStackIterator(std.debug.StackIterator.init(@returnAddress(), @frameAddress()));
    printRegisters(frame);

    if ((frame.iret.cs & 0x3) != 0 and cpu_info.currentProcess() != null) {
        // TODO: Implement signals and stuff like that so we can properly
        // terminate processes that violate memory protections
        const process = cpu_info.currentProcess().?;
        logger.info("Killed {}:{} because of a protection violation", .{ process.pid, cpu_info.thread.?.tid });
        scheduler.exitProcess(process, 0xff);
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

const swapgs_if_needed = std.fmt.comptimePrint(
    \\
    \\testb $0x3, 0x{X}(%%rsp)
    \\je 1f
    \\swapgs
    \\1:
    \\
, .{@offsetOf(IretFrame, "cs")});

fn makeHandler(comptime vector: usize) InterruptStub {
    // https://wiki.osdev.org/Exceptions
    const has_error_code = switch (vector) {
        0x8 => true,
        0xA...0xE => true,
        0x11 => true,
        0x15 => true,
        0x1D...0x1E => true,
        else => false,
    };

    return struct {
        fn handler() callconv(.Naked) void {
            if (has_error_code) {
                asm volatile (swapgs_if_needed ++
                        \\pushq %[vector]
                        \\jmp interruptCommonHandler
                    :
                    : [vector] "i" (vector),
                      [_] "i" (interruptCommonHandler),
                );
            } else {
                asm volatile (
                    \\pushq $0
                    ++ swapgs_if_needed ++
                        \\pushq %[vector]
                        \\jmp interruptCommonHandler
                    :
                    : [vector] "i" (vector),
                      [_] "i" (interruptCommonHandler),
                );
            }
        }
    }.handler;
}

export fn interruptHandler(frame: *InterruptFrame) callconv(.C) void {
    const handler = handlers[frame.vector & 0xFF];
    handler(frame);
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
        \\mov %%rsp, %%rdi
        \\call interruptHandler
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
        \\add $8, %%rsp
        ++ swapgs_if_needed ++
            \\add $8, %%rsp
            \\iretq
        :
        : [_] "i" (interruptHandler),
    );
}
