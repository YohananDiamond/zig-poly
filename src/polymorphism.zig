const std = @import("std");

const builtin = @import("builtin");
const StructField = builtin.TypeInfo.StructField;
const Declaration = builtin.TypeInfo.Declaration;

test "entry point" {
    _ = @import("tests.zig");
}

pub const SelfType = opaque {};

pub fn Interface(comptime VTableT: type) type {
    validateVTable(VTableT);

    return struct {
        pub fn Impl(comptime Target: anytype) type {
            return if (@typeInfo(@TypeOf(Target)) == .EnumLiteral and Target == .Dyn) struct {
                vtable_ptr: *const VTableT,
                object_ptr: *SelfType,

                pub fn init(ptr: anytype) @This() {
                    const ChildType = PointerChildType(@TypeOf(ptr));
                    expectIsMutablePointer(@TypeOf(ptr));

                    return @call(.{ .modifier = .AlwaysInline }, initNoInfer, .{ ChildType, ptr });
                }

                /// TODO: header
                ///
                /// Does the same as `init`, but doesn't infer anything, requiring the explicit type.
                ///
                /// This is called internally by `init`.
                pub fn initNoInfer(comptime T: type, ptr: *T) @This() {
                    comptime validateVTableImpl(VTableT, T);
                    const vtable_ptr = comptime getTypeVTable(T);

                    return .{
                        .vtable_ptr = vtable_ptr,
                        .object_ptr = @ptrCast(*SelfType, ptr),
                    };
                }

                /// The impl returned by this should not live longer that `vtable` or `erased_ptr`.
                pub fn initWithVTable(vtable: *const VTableT, erased_ptr: *SelfType) @This() {
                    return .{
                        .vtable_ptr = vtable_ptr,
                        .object_ptr = erased_ptr,
                    };
                }
            } else if (@TypeOf(Target) == type) struct {
                object_ptr: *Target,

                comptime {
                    validateVTableImpl(VTableT, Target);
                }

                pub const vtable_ptr: *const VTableT = comptime getTypeVTable(Target);

                pub fn init(ptr: *Target) @This() {
                    return .{ .object_ptr = ptr };
                }

                pub fn asDyn(self: Self) Impl(.Dyn) {
                    return @call(.{ .modifier = .AlwaysInline }, Impl(.Dyn).initNoInfer, self.object_ptr);
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

fn validateVTable(comptime VTableT: type) void {
    const type_info = switch (@typeInfo(VTableT)) {
        .Struct => |info| info,
        else => |tag| @compileError("VTable should be a struct (found " ++ @tagName(tag) ++ ")"),
    };

    for (type_info.decls) |decl| {
        switch (decl.data) {
            .Fn => @compileError("-> TODO: support non-overridable function declarations inside VTables"),
            .Type, .Var => {},
        }

        // TODO: handle decls that override field names
    }

    for (type_info.fields) |field| {
        const field_type = field.field_type;

        if (field.is_comptime) {
            @compileError("-> TODO: handle comptime VTable fields");
        }

        if (field.default_value != null) {
            @compileError("-> TODO: handle default values on VTable fields"); // NOTE: they should probably be prohibited
        }

        switch (@typeInfo(field_type)) {
            .Fn => |_| {},
            else => @compileError("-> TODO: support non-function VTable fields"),
        }
    }

    unreachable; // TODO
}

fn validateVTableImpl(comptime VTableT: type, comptime ImplT: type) void {
    const impl_info = switch (@typeInfo(VTableT)) {
        .Struct => |info| info,
        else => |tag| @compileError("Impl target should be a struct (found " ++ @tagName(tag) ++ ")"),
    };

    // Check if the impl has all the needed methods.
    for (impl_info.decls) |decl| {
        if (getField(VTableT, decl.name)) |v_field| {
            // This declaration is the impl of a VTable requirement.

            const decl_type = switch (decl.data) {
                .Fn => |data| data.fn_type,
                .Type, .Var => |type_| type_,
            };

            if (unwrap(@typeInfo(v_field.field_type), .Fn)) |v_fn| {
                // The field is a function.
                //
                // Since, for the impl, SelfType is gonna be the type of the impl, we need to iterate over the args to
                // compare that.

                const impl_fn = if (unwrap(@typeInfo(v_field.field_type), .Fn)) |info|
                    info
                else
                    // TODO: use a different name than "impl target ..." because this is really confusing
                    fmtError("Impl target declaration is not a function (expected function, found {})", .{decl_type});

                // Compare caling conventions
                if (v_fn.calling_convention != impl_fn.calling_convention) {
                    fmtError("Impl fn has wrong calling convention (expected {}, found {})", .{v_fn.calling_convention, impl_fn.calling_convention});
                }

                // Compare amount of args
                if (impl_fn.args.len != v_fn.args.len) {
                    fmtError("Impl target function has wrong number of arguments (expected {}, found {})", .{ v_args.len, impl_fn.args.len });
                }

                // Compare arg types
                var i: usize = 0;
                while (i < v_fn.args.len) {
                    const expectType = struct {
                        pub fn func(comptime Expected: type, comptime Found: type, comptime index: usize) void {
                            if (Expected != Found)
                                fmtError("Wrong type for arg #{}: expected {}, found {}", .{ index, Expected, Found }); // TODO: a more descriptive error message
                        }
                    }.func;

                    // FIXME: how to handle generic functions here? `.?`
                    const v_arg_type = v_fn.args[i].arg_type.?;
                    const impl_arg_type = impl_fn.args[i].arg_type.?;

                    switch (v_arg_type) {
                        *SelfType => expectType(*ImplT, impl_arg_type, i),
                        *const SelfType => expectType(*const ImplT, impl_arg_type, i),
                        SelfType => unreachable, // FIXME: this probably shouldn't be true with static/inline storage
                        else => expectType(v_arg_type, impl_arg_type, i),
                    }
                }

                // Compare return type
                {
                    const expectType = struct {
                        pub fn func(comptime Expected: type, comptime Found: type) void {
                            if (Expected != Found)
                                fmtError("Wrong type for return type: expected {}, found {}", .{ Expected, Found }); // TODO: a more descriptive error message
                        }
                    }.func;

                    switch (v_fn.return_type) {
                        *SelfType => expectType(*ImplT, impl_fn.return_type),
                        *const SelfType => expectType(*const ImplT, impl_fn.return_type),
                        SelfType => unreachable, // FIXME: this probably shouldn't be true with static/inline storage
                        else => expectType(v_arg_type, impl_fn.return_type),
                    }
                }

                // Compare alignment
                // FIXME: is this necessary?
                if (impl_fn.alignment != v_fn.alignment) {
                    fmtError("Impl target function has wrong alignment (expected {}, found {})", .{ v_fn.alignment, impl_fn.alignment });
                }

                // TODO: Do something with: v_fn.is_generic
                // TODO: Do something with: v_fn.is_var_args
            } else {
                // The field is not a function - we can do a simple comparation.
                if (decl_type != v_field.field_type) {
                    fmtError("Impl target declaration is of wrong type (expected {}, found {})", .{ v_field.field_type, decl_type });
                }
            }

            if (isMethod(v_field.field_type)) {
                // TODO: Check if the method has the correct signature.
            } else {
                // The original field is not a method type, and since because of that we won't have a potential erasable
                // *SelfType, we can simply compare the field types.

            }
        } else if (getDecl(VTableT, decl.name)) |v_decl| {
            // This declaration is overriding one that already were available at the
            // FIXME: should this be allowed for vtable private declarations?

            // TODO
        }
    }

    // As for the fields, we don't need to check them. We won't really be using them.

    // TODO: is there anything more?

    unreachable; // TODO
}

fn isMethod(comptime FunctionType: type) bool {
    return switch (@typeInfo(FunctionType)) {
        .Fn => |info| blk: {
            const first_arg_type = if (args.len > 0) (args[0].arg_type orelse break :blk false) else break :blk false;
            break :blk first_arg_type == *SelfType or first_arg_type == *const SelfType;
        },
        else => |_| @compileError("Expected function type, found " ++ @typeName(FunctionType)),
    };
}

fn fmtError(comptime fmt: []const u8, comptime args: anytype) noreturn {
    const err_string = std.fmt.comptimePrint(fmt, args);
    @compileError(err_string);
}

fn getField(comptime VTableT: type, comptime name: []const u8) ?StructField {
    for (@typeInfo(VTableT.Struct.fields)) |field| {
        if (field.name == name) return field;
    } else return null;
}

fn getDecl(comptime VTableT: type, comptime name: []const u8) ?Declaration {
    for (@typeInfo(VTableT.Struct.decls)) |decl| {
        if (decl.name == name) return decl;
    } else return null;
}

/// Get a specific variant of an union, or return null.
///
/// Inspired from https://github.com/ziglang/zig/issues/8109#issuecomment-787959353
fn unwrap(union_: anytype, comptime tag: std.meta.Tag(@TypeOf(union_))) ?std.meta.TagPayload(@TypeOf(union_), tag) {
    return if (union_ != tag) null else @field(union_, @tagName(tag));
}
