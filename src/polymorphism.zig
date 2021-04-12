const std = @import("std");

test "entry point" {
    _ = @import("tests.zig");
}

pub const SelfType = opaque {};

pub fn Interface(comptime VTableT: type) type {
    return struct {
        pub fn Impl(comptime Target: anytype) type {
            return if (@typeInfo(@TypeOf(Target)) == .EnumLiteral and Target == .Dyn) struct {
                vtable_ptr: *const VTableT,
                object_ptr: *SelfType,

                pub fn init(ptr: anytype) @This() {
                    const ChildType = PointerChildType(@TypeOf(ptr));
                    expectIsMutablePointer(@TypeOf(ptr));

                    comptime validateVtable(ChildType);
                    const vtable_ptr = comptime getTypeVTable(ChildType);

                    return .{
                        .vtable_ptr = vtable_ptr,
                        .object_ptr = @ptrCast(*SelfType, ptr),
                    };
                }
            } else if (@TypeOf(Target) == type) struct {
                object_ptr: *Target,

                // comptime {
                //     validateVtable(Target);
                // }

                pub const vtable_ptr: *const VTableT = comptime getTypeVTable(Target);

                pub fn init(ptr: *Target) @This() {
                    return .{ .object_ptr = ptr };
                }
            } else {
                @compileError("Invalid impl target (expected either a type or .Dyn, found " ++ @typeName(@TypeOf(Target)) ++ ")");
            };
        }

        pub fn staticInit(ptr: anytype) Impl(PointerChildType(@TypeOf(ptr))) {
            const ChildType = PointerChildType(@TypeOf(ptr));
            expectIsMutablePointer(@TypeOf(ptr));

            return Impl(ChildType).init(ptr);
        }

        pub const dynamicInit = Impl(.Dyn).init;
    };
}

fn PointerChildType(comptime PointerType: type) type {
    return switch (@typeInfo(PointerType)) {
        .Pointer => |info| info.child,
        else => @compileError("Expected pointer, found " ++ @typeName(PointerType)),
    };
}

fn expectIsMutablePointer(comptime PointerType: type) void {
    if (@typeInfo(PointerType).Pointer.is_const) {
        @compileError("Expected mutable pointer, found const pointer");
    }
}

fn validateVtable(comptime VTableT: type) void {
    unreachable;
}
