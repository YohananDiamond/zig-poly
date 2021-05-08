const std = @import("std");
const meta = std.meta;

const builtin = @import("builtin");
const StructField = builtin.TypeInfo.StructField;
const Declaration = builtin.TypeInfo.Declaration;

const utils = @import("utils.zig");
const fmtError = utils.fmtError;
const identQuote = utils.identQuote;
const unwrapUnion = utils.unwrapUnion;

test "entry point" {
    // Tests
    _ = @import("tests/entry.zig");

    // Related modules & stuff
    std.testing.refAllDecls(@This());
}

// TODO: proper error messages for every compile-error here

pub const SelfType = opaque {};

pub const InterfaceOptions = struct {
    async_call_stack_size: usize = 1 * 1024 * 1024,
};

pub fn Interface(comptime VTableT: type, comptime options: InterfaceOptions) type {
    validateVTable(VTableT);

    return struct {
        const IFaceSelf = @This();

        pub const VTableType = VTableT;
        pub const interface_options = options;

        pub fn isBaseOf(comptime IType: type) bool { // TODO: terminology: define "base"
            comptime {
                const field_name = "_InterfaceBaseType";

                if (!meta.trait.is(.Struct)(IType)) return false;
                if (!@hasDecl(IType, field_name)) return false;
                if (@TypeOf(@field(IType, field_name)) != type) return false;
                return @field(IType, field_name) == IFaceSelf;
            }
        }

        pub fn Impl(comptime target_: anytype) type {
            const TargetKind = union(enum) {
                Dynamic: void,
                Static: type,
            };

            const target: TargetKind = switch (@typeInfo(@TypeOf(target_))) {
                .EnumLiteral => if (target_ == .Dyn) .Dynamic else {
                    fmtError("Invalid impl target (expected .Dyn or a type, found .{})", .{@tagName(target_)});
                },
                .Type => .{ .Static = target_ },
                else => fmtError("Invalid impl target (expected .Dyn or a type, found {})", .{@TypeOf(target_)}),
            };

            return if (unwrapUnion(target, .Dynamic)) |_| struct {
                vtable_ptr: *const VTableT,
                object_ptr: *SelfType,

                pub const _InterfaceBaseType = IFaceSelf;

                pub fn init(ptr: anytype) @This() {
                    const ChildType = PointerChildType(@TypeOf(ptr));
                    expectIsMutablePointer(@TypeOf(ptr));

                    return @call(.{ .modifier = .always_inline }, initSpecific, .{ ChildType, ptr });
                }

                /// Initializes the interface while specifying a specific type.
                ///
                /// Does the same as `init`, but doesn't infer anything, requiring the explicit type.
                ///
                /// This is called internally by `init`.
                pub fn initSpecific(comptime T: type, ptr: *T) @This() {
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

                /// Call a function from the VTable, using dynamic dispatching.
                ///
                /// Will automatically call as a method if the function is one.
                ///
                /// Note: not all functions can be called via dynamic dispatch - see { TODO } for more details.
                pub fn call(
                    self: @This(),
                    comptime fn_name: []const u8,
                    args: anytype,
                ) callconv(.Inline) VTableFuncReturnType(VTableT, fn_name) {
                    // VTableFuncReturnType already asserts the following type is a function, so we don't need to worry
                    // about that.
                    const func_type = comptime VTableFieldType(VTableT, fn_name);
                    const reflection = comptime FnReflection.analyze(func_type);

                    comptime if (reflection.requires_static_dispatching) fmtError(
                        "Attempted to call static-dispatching-only method {s} (of type {}) with dynamic dispatching",
                        .{ identQuote(fn_name), func_type },
                    );

                    const fn_ptr = @field(self.vtable_ptr, fn_name);

                    if (comptime isMethod(func_type)) {
                        return @call(.{}, fn_ptr, .{self.object_ptr} ++ args);
                    } else {
                        return @call(.{}, fn_ptr, args);
                    }
                }
            } else if (unwrapUnion(target, .Static)) |Target| struct {
                object_ptr: *Target,

                comptime {
                    validateVTableImpl(VTableT, Target);
                }

                pub const _InterfaceBaseType = IFaceSelf;
                pub const vtable_ptr: *const VTableT = comptime vTableGetImpl(VTableT, Target);

                pub fn init(ptr: *Target) @This() {
                    return .{ .object_ptr = ptr };
                }

                pub fn asDyn(self: Self) Impl(.Dyn) {
                    return @call(.{ .modifier = .AlwaysInline }, Impl(.Dyn).initSpecific, self.object_ptr);
                }

                /// Call a function from the VTable, using static dispatching.
                ///
                /// Will automatically call as a method if the function is one.
                ///
                /// Note: not all functions can be called via dynamic dispatch - see { TODO } for more details.
                pub fn call(
                    self: @This(),
                    comptime name: []const u8,
                    args: anytype,
                ) callconv(.Inline) VTableFuncReturnType(VTableT, name) {
                    // VTableFuncReturnType already asserts the following type is a function, so we don't need to worry
                    // about that.
                    const func_type = comptime VTableFieldType(VTableT, name);

                    const fn_ptr = comptime @field(vtable_ptr, name);

                    if (comptime isMethod(func_type)) {
                        return @call(.{}, fn_ptr, .{self.object_ptr} ++ args); // FIXME: how and why is SelfType automatically casting to the specialized types?
                    } else {
                        return @call(.{}, fn_ptr, args);
                    }
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
        else => |tag| fmtError("VTable {} should be a struct (found {s})", .{ VTableT, identQuote(@tagName(tag)) }),
    };

    for (v_info.decls) |decl| {
        switch (decl.data) {
            .Fn => |fn_data| if (isMethod(fn_data.fn_type)) {
                fmtError("VTable {}: function decls inside must not be methods (found {s})", .{ VTableT, identQuote(decl.name) });
            },
            .Type, .Var => {},
        }

        // TODO: handle decls that override field names
    }

    for (v_info.fields) |field| {
        // TODO: handle optional fields

        for (v_info.decls) |decl| {
            if (decl.is_pub and decl.name == field.name) {
                fmtError("VTable {}: field {s} conflicts with public declaration of same name", .{ VTableT, identQuote(field.name) });
            }
        }

        const field_type = field.field_type;

        if (field.is_comptime) {
            fmtError("VTable {}: fields cannot not be comptime (found {s})", .{ VTableT, identQuote(field.name) });
        }

        if (field.default_value != null) {
            fmtError("VTable {}: TODO: handle default values (found at field {s})", .{ VTableT, identQuote(field.name) });
        }

        switch (@typeInfo(field_type)) {
            .Fn => |fn_info| {
                if (fn_info.is_generic) {
                    fmtError("VTable {}: TODO: support generic functions (from field {s})", .{ VTableT, field.name });
                }

                switch (fn_info.calling_convention) {
                    .Unspecified => {},
                    else => fmtError("VTable {}: TODO: support more calling conventions", .{VTableT}),
                }
            },
            else => fmtError("VTable {}: TODO: support non-function VTable fields", .{VTableT}),
        }
    }
}

/// Specialize `PossibleSelf` to a variant of `SpecializationTarget`, if possible.
fn SelfTypeSpecializeOnto(comptime SpecializationTarget: type, comptime PossibleSelf: type) type {
    return switch (PossibleSelf) {
        SelfType => SpecializationTarget,
        *SelfType => *SpecializationTarget,
        *const SelfType => *const SpecializationTarget,
        else => PossibleSelf,
    };
}

fn validateVTableImpl(comptime VTableT: type, comptime ImplT: type) void {
    const v_info = @typeInfo(VTableT).Struct;

    const impl_info_new = switch (@typeInfo(ImplT)) {
        .Struct => |info| info,
        else => |tag| fmtError(
            "VTable {}: impl type {} is not a struct (found {s})",
            .{ VTableT, ImplT, @tagName(tag) },
        ),
    };

    // Check if the impl has all the needed methods.
    for (impl_info_new.decls) |impl_decl| {
        if (getField(VTableT, impl_decl.name)) |v_field| {
            // This declaration is the impl of a VTable requirement.

            const impl_decl_type = switch (impl_decl.data) {
                .Fn => |data| data.fn_type,
                .Type, .Var => |type_| type_,
            };

            if (unwrapUnion(@typeInfo(v_field.field_type), .Fn)) |v_fn| {
                // The field is a function.
                //
                // Since, for the impl, SelfType is gonna be the type of the impl, we need to iterate over the args to
                // compare that.

                const impl_fn = if (unwrapUnion(@typeInfo(impl_decl_type), .Fn)) |info|
                    info
                else
                    // TODO: use a different name than "impl target ..." because this is really confusing
                    fmtError("VTable {}: impl target declaration is not a function (expected function, found {})", .{ VTableT, decl_type });

                // Compare caling conventions
                if (v_fn.calling_convention != impl_fn.calling_convention) {
                    fmtError("VTable {}: impl fn has wrong calling convention (expected {}, found {})", .{ VTableT, v_fn.calling_convention, impl_fn.calling_convention });
                }

                // Compare amount of args
                if (impl_fn.args.len != v_fn.args.len) {
                    fmtError("VTable {}: impl target function has wrong number of arguments (expected {}, found {})", .{ VTableT, v_fn.args.len, impl_fn.args.len });
                }

                // Compare arg types
                var i: usize = 0;
                while (i < v_fn.args.len) : (i += 1) {
                    const v_arg_type = SelfTypeSpecializeOnto(ImplT, v_fn.args[i].arg_type.?); // FIXME: generic unreachable
                    const impl_arg_type = impl_fn.args[i].arg_type.?; // FIXME: generic unreachable

                    if (v_arg_type != impl_arg_type) fmtError(
                        "VTable {}: mismatched types for arg #{}: expected {}, found {}",
                        .{ VTableT, i, v_arg_type, impl_arg_type },
                    );

                    // const expectType = struct {
                    //     pub fn func(comptime Expected: type, comptime Found: type, comptime index: usize) void {
                    //         if (Expected != Found) fmtError(
                    //             "VTable {}: wrong type for arg #{}: expected {}, found {}",
                    //             .{ VTableT, index, Expected, Found },
                    //         ); // TODO: a more descriptive error message
                    //     }
                    // }.func;

                    // switch (v_arg_type) {
                    //     *SelfType => expectType(*ImplT, impl_arg_type, i),
                    //     // *SelfType => expectType(*ImplT, impl_arg_type, i),
                    //     *const SelfType => expectType(*const ImplT, impl_arg_type, i),
                    //     SelfType => unreachable, // FIXME: this probably shouldn't be true with static/inline storage
                    //     else => expectType(v_arg_type, impl_arg_type, i),
                    // }
                }

                // Compare return type
                {
                    const v_return_type = SelfTypeSpecializeOnto(ImplT, v_fn.return_type.?); // FIXME: generic unreachable
                    const impl_return_type = impl_fn.return_type.?; // FIXME: generic unreachable

                    if (v_return_type != impl_return_type) fmtError(
                        "VTable {}: mismatched return types: expected {}, found {}",
                        .{ VTableT, v_return_type, impl_return_type },
                    );

                    // const expectType = struct {
                    //     pub fn func(comptime Expected: type, comptime Found: type) void {
                    //         if (Expected != Found)
                    //             fmtError("VTable {}: wrong type for return type: expected {}, found {}", .{ VTableT, Expected, Found }); // TODO: a more descriptive error message
                    //     }
                    // }.func;

                    // switch (v_fn.return_type.?) {
                    //     *SelfType => expectType(*ImplT, impl_fn.return_type),
                    //     *const SelfType => expectType(*const ImplT, impl_fn.return_type),
                    //     SelfType => unreachable, // FIXME: this probably shouldn't be true with static/inline storage
                    //     else => expectType(v_fn.return_type.?, impl_fn.return_type.?),
                    // }
                }

                // Compare alignment
                // FIXME: is this necessary?
                if (impl_fn.alignment != v_fn.alignment) {
                    fmtError("VTable {}: impl target function has wrong alignment (expected {}, found {})", .{ VTableT, v_fn.alignment, impl_fn.alignment });
                }

                // TODO: Do something with: v_fn.is_generic
                // TODO: Do something with: v_fn.is_var_args
            } else {
                // The field is not a function - we can do a simple comparation.
                if (decl_type != v_field.field_type) {
                    fmtError("VTable {}: impl target declaration is of wrong type (expected {}, found {})", .{ VTableT, v_field.field_type, decl_type });
                }
            }
        } else if (getDecl(VTableT, impl_decl.name)) |v_decl| {
            // This declaration is overriding one that already was defined in the VTable.
            //
            // We can allow this if the VTable declaration is private, since it is only used inside the VTable
            // declaration.
            if (v_decl.is_pub) {
                fmtError("VTable {}: impl target declaration {s} overrides public declaration of same name in VTable type", .{ VTableT, impl_decl.name });
            }
        }
    }

    // As for the fields, we don't need to check them. We won't really be using them.

    // TODO: is there anything more?
}

fn isMethod(comptime FunctionType: type) bool {
    const info = comptime @typeInfo(FunctionType).Fn;

    if (info.args.len == 0)
        return false;

    const first_arg_type = info.args[0].arg_type.?;
    return first_arg_type == *SelfType or first_arg_type == *const SelfType;
}

fn getField(comptime VTableT: type, comptime name: []const u8) ?StructField {
    comptime {
        for (@typeInfo(VTableT).Struct.fields) |field| {
            if (std.mem.eql(u8, field.name, name))
                return field;
        }

        return null;
    }
}

fn getDecl(comptime VTableT: type, comptime name: []const u8) ?Declaration {
    comptime {
        for (@typeInfo(VTableT).Struct.decls) |decl| {
            if (decl.name == name)
                return decl;
        }

        return null;
    }
}

fn makeSelfPtr(comptime Source: type, ptr: *Source) *SelfType {
    return if (@sizeOf(Source) > 0) @ptrCast(*SelfType, ptr) else undefined;
}

fn VTableFieldType(comptime VTableT: type, comptime name: []const u8) type {
    const FieldsT = meta.FieldEnum(VTableT);

    return meta.fieldInfo(VTableT, @field(FieldsT, name)).field_type;
}

fn VTableFuncReturnType(comptime VTableT: type, comptime name: []const u8) type {
    const field_type = VTableFieldType(VTableT, name);

    return switch (@typeInfo(field_type)) {
        .Fn => |info| info.return_type.?, // FIXME: generic unreachable
        else => fmtError("VTable {}: field {s} is not a function (found {})", .{ VTableT, identQuote(name), field_type }),
    };
}

/// NOTE: only call this if VTableT is already verified and ImplT is verified as a member of VTableT
fn vTableGetImpl(comptime VTableT: type, comptime ImplT: type) *const VTableT {
    comptime {
        var vtable: VTableT = undefined;

        for (@typeInfo(VTableT).Struct.fields) |v_field| {
            const FieldType = @TypeOf(@field(vtable, v_field.name));

            const impl_decl = if (@hasDecl(ImplT, v_field.name))
                @field(ImplT, v_field.name)
            else
                fmtError("VTable {}: impl type {} does not have required decl: {s}", .{ VTableT, ImplT, identQuote(v_field.name) });

            @field(vtable, v_field.name) = if (meta.trait.is(.Fn)(FieldType))
                @ptrCast(@TypeOf(@field(vtable, v_field.name)), impl_decl)
            else
                impl_decl;
        }

        return &vtable;
    }
}

const FnReflection = struct {
    is_generic: bool,
    requires_inline_storage: bool,
    requires_static_dispatching: bool,

    /// Analyze a function type and create an instance of this struct.
    ///
    /// Should only be used with VTable function fields (that doesn't include the impls!).
    pub fn analyze(comptime FnType: type) @This() {
        const info = @typeInfo(FnType).Fn;

        var requires_inline_storage = false;
        var requires_static_dispatching = info.is_generic;

        const is_method = info.args.len > 0 and if (info.args[0].arg_type) |arg_type|
            switch (arg_type) {
                *const SelfType, *SelfType => true,
                SelfType => blk: {
                    requires_inline_storage = true;
                    break :blk true;
                },
                else => false,
            }
        else blk: {
            requires_static_dispatching = true;
            break :blk false;
        };

        for (info.args[1..]) |arg| {
            if (arg.arg_type) |arg_type|
                switch (arg_type) {
                    SelfType, *const SelfType, *SelfType => {
                        requires_static_dispatching = true; // TODO: add a "reason" variable for these checks
                    },
                    else => {},
                }
            else {
                requires_static_dispatching = true;
            }
        }

        return @This(){
            .is_generic = info.is_generic,
            .requires_inline_storage = requires_inline_storage,
            .requires_static_dispatching = requires_static_dispatching,
        };
    }
};
