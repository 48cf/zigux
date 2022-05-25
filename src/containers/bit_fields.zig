const std = @import("std");

fn PtrCastPreserveCV(comptime T: type, comptime PtrToT: type, comptime NewT: type) type {
    return switch (PtrToT) {
        *T => *NewT,
        *const T => *const NewT,
        *volatile T => *volatile NewT,
        *const volatile T => *const volatile NewT,

        else => @compileError("wtf you doing"),
    };
}

fn BitType(comptime FieldType: type, comptime ValueType: type, comptime shamt: usize) type {
    const self_bit: FieldType = (1 << shamt);

    return struct {
        bits: BitField(FieldType, shamt, 1),

        pub fn set(self: anytype) void {
            self.bits.field().* |= self_bit;
        }

        pub fn unset(self: anytype) void {
            self.bits.field().* &= ~self_bit;
        }

        pub fn read(self: anytype) ValueType {
            return @bitCast(ValueType, @truncate(u1, self.bits.field().* >> shamt));
        }

        // Since these are mostly used with MMIO, I want to avoid
        // reading the memory just to write it again, also races
        pub fn write(self: anytype, val: ValueType) void {
            if (@bitCast(bool, val)) {
                self.set();
            } else {
                self.unset();
            }
        }
    };
}

pub fn Bit(comptime FieldType: type, comptime shamt: usize) type {
    return BitType(FieldType, u1, shamt);
}

pub fn Boolean(comptime FieldType: type, comptime shamt: usize) type {
    return BitType(FieldType, bool, shamt);
}

pub fn BitField(comptime FieldType: type, comptime shamt: usize, comptime num_bits: usize) type {
    if (shamt + num_bits > @bitSizeOf(FieldType)) {
        @compileError("bit field doesn't fit");
    }

    const self_mask: FieldType = ((1 << num_bits) - 1) << shamt;

    const ValueType = std.meta.Int(.unsigned, num_bits);

    return struct {
        dummy: FieldType,

        fn field(self: anytype) PtrCastPreserveCV(@This(), @TypeOf(self), FieldType) {
            return @ptrCast(PtrCastPreserveCV(@This(), @TypeOf(self), FieldType), self);
        }

        pub fn write(self: anytype, val: ValueType) void {
            self.field().* &= ~self_mask;
            self.field().* |= @intCast(FieldType, val) << shamt;
        }

        pub fn read(self: anytype) ValueType {
            const val: FieldType = self.field().*;
            return @intCast(ValueType, (val & self_mask) >> shamt);
        }
    };
}
