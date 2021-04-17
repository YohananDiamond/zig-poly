const std = @import("std");
const meta = std.meta;

const builtin = @import("builtin");
const StructField = builtin.TypeInfo.StructField;
const Declaration = builtin.TypeInfo.Declaration;

const utils = @import("utils.zig");
const fmtError = utils.fmtError;
const unwrapUnion = utils.unwrapUnion;

test "entry point" {
    _ = @import("tests.zig");
}

pub const SelfType = opaque {};

pub const InterfaceOptions = struct {
    async_call_stack_size: usize = 1 * 1024 * 1024,
};

pub fn Interface(comptime VTableT: type, comptime options: InterfaceOptions) type {
    validateVTable(VTableT);

    return struct {
        pub fn Impl(comptime _target: anytype) type {
            const TargetType = union(enum) {
                Dynamic: void,
                Static: type,
            };

            const target: TargetType = switch (@typeInfo(@TypeOf(_target))) {
                .EnumLiteral => if (_target == .Dyn) .{ .Dynamic = {} } else {
                    fmtError("Invalid impl target (expected .Dyn or type, found .{})", .{@tagName(_target)});
                },
                .Type => .{ .Static = _target },
                else => fmtError("Invalid impl target (expected .Dyn or type, found {})", .{@TypeOf(_target)}),
            };

            return if (unwrapUnion(target, .Dynamic)) |_| struct {
                vtable_ptr: *const VTableT,
                object_ptr: *SelfType,

                pub fn init(ptr: anytype) @This() {
                    const ChildType = PointerChildType(@TypeOf(ptr));
                    expectIsMutablePointer(@TypeOf(ptr));

                    return @call(.{ .modifier = .always_inline }, initNoInfer, .{ ChildType, ptr });
                }

                /// TODO: header
                ///
                /// Does the same as `init`, but doesn't infer anything, requiring the explicit type.
                ///
                /// This is called internally by `init`.
                pub fn initNoInfer(comptime T: type, ptr: *T) @This() {
                    comptime validateVTableImpl(VTableT, T);
                    const vtable_ptr = comptime vTableGetImpl(VTableT, T);

                    return .{
                        .vtable_ptr = vtable_ptr,
                        .object_ptr = makeSelfPtr(T, ptr),
                    };
                }

                /// The impl returned by this should not live longer that `vtable` or `erased_ptr`.
                pub fn initWithVTable(vtable: *const VTableT, erased_ptr: *SelfType) @This() {
                    return .{
                        .vtable_ptr = vtable_ptr,
                        .object_ptr = erased_ptr,
                    };
                }
            } else if (unwrapUnion(target, .Static)) |Target| struct {
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
            } else unreachable;
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
    const v_info = switch (@typeInfo(VTableT)) {
        .Struct => |v_info| v_info,
        else => |tag| @compileError("VTable should be a struct (found " ++ @tagName(tag) ++ ")"),
    };

    for (v_info.decls) |decl| {
        switch (decl.data) {
            .Fn => |f| if (isMethod(f.fn_type)) {
                fmtError("VTable non-overridable function {s} must not be a method", .{decl.name});
                // TODO: do this same error if any `*SelfType` argument is found... for now
            } else {
                non_overridable_funcs = non_overridable_funcs ++ decl.name;
            },
            .Type, .Var => {},
        }

        // TODO: handle decls that override field names
    }

    for (v_info.fields) |field| {
        // TODO: handle optional fields

        for (v_info.decls) |decl| {
            if (decl.is_pub and decl.name == field.name) {
                fmtError("VTable field {s} conflicts with public declaration of same name", .{field.name});
            }
        }

        const field_type = field.field_type;

        if (field.is_comptime) {
            @compileError("-> TODO: handle comptime VTable fields");
        }

        if (field.default_value != null) {
            @compileError("-> TODO: handle default values on VTable fields");
        }

        switch (@typeInfo(field_type)) {
            .Fn => |fn_info| {
                if (fn_info.is_generic) {
                    fmtError("-> TODO: support generic functions ({})", .{field.name});
                }

                switch (fn_info.calling_convention) {
                    .Unspecified, .Async => {},
                    else => @compileError("-> TODO: support more calling conventions"),
                }
            },
            else => @compileError("-> TODO: support non-function VTable fields"),
        }
    }
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

            if (unwrapUnion(@typeInfo(v_field.field_type), .Fn)) |v_fn| {
                // The field is a function.
                //
                // Since, for the impl, SelfType is gonna be the type of the impl, we need to iterate over the args to
                // compare that.

                const impl_fn = if (unwrapUnion(@typeInfo(decl_type), .Fn)) |info|
                    info
                else
                    // TODO: use a different name than "impl target ..." because this is really confusing
                    fmtError("Impl target declaration is not a function (expected function, found {})", .{decl_type});

                // Compare caling conventions
                if (v_fn.calling_convention != impl_fn.calling_convention) {
                    fmtError("Impl fn has wrong calling convention (expected {}, found {})", .{ v_fn.calling_convention, impl_fn.calling_convention });
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
        } else if (getDecl(VTableT, decl.name)) |v_decl| {
            // This declaration is overriding one that already was defined in the VTable.
            //
            // We can allow this if the VTable declaration is private, since it is only used inside the VTable
            // declaration.
            if (v_decl.is_pub) {
                fmtError("Impl target declaration {s} overrides public declaration of same name in VTable type", .{decl.name});
            }
        }
    }

    // As for the fields, we don't need to check them. We won't really be using them.

    // TODO: is there anything more?
}

fn isMethod(comptime FunctionType: type) bool {
    return switch (@typeInfo(FunctionType)) {
        .Fn => |info| blk: {
            const first_arg_type = if (info.args.len > 0) info.args[0].arg_type.? else break :blk false;
            break :blk first_arg_type == *SelfType or first_arg_type == *const SelfType;
        },
        else => |_| @compileError("Expected function type, found " ++ @typeName(FunctionType)),
    };
}

fn getField(comptime VTableT: type, comptime name: []const u8) ?StructField {
    for (@typeInfo(VTableT.Struct.fields)) |field| {
        if (field.name == name) return field;
    } else return null;
}

fn getDecl(comptime VTableT: type, comptime name: []const u8) ?Declaration {
    for (@typeInfo(VTableT).Struct.decls) |decl| {
        if (decl.name == name) return decl;
    } else return null;
}

fn makeSelfPtr(comptime Source: type, ptr: *Source) *SelfType {
    return if (@sizeOf(Source) > 0) @ptrCast(*SelfType, ptr)
    else
        undefined;
}

/// NOTE: only call this if VTableT is already verified and ImplT is verified as a member of VTableT
fn vTableGetImpl(comptime VTableT: type, comptime ImplT: type) *const VTableT {
    return comptime blk: {
        var vtable: VTableT = undefined;

        for (@typeInfo(VTableT).Struct.fields) |v_field| {
            const FieldType = @TypeOf(@field(vtable, v_field.name));
            const impl_decl = @field(ImplT, v_field.name);

            @field(vtable, v_field.name) = if (meta.trait.is(.Fn)(FieldType))
                @ptrCast(@TypeOf(@field(vtable, v_field.name)), impl_decl)
            else
                impl_decl;
        }

        break :blk &vtable;
    };
}
