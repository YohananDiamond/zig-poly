const std = @import("std");
const testing = std.testing;

const poly = @import("../poly.zig");

const MagicNumber = poly.Interface(struct {
    getMagicNumber: fn (self: *poly.SelfType) u8,
}, .{});

const Five = struct {
    const Self = @This();

    pub fn getMagicNumber(self: *Self) u8 {
        return 5;
    }
};

const Six = struct {
    const Self = @This();

    pub fn getMagicNumber(self: *Self) u8 {
        return 6;
    }
};

test "simple static dispatching" {
    var five_n = Five{};
    const magic_num = MagicNumber.staticInit(&five_n);

    try testing.expect(magic_num.call("getMagicNumber", .{}) == 5);
}

test "simple dynamic dispatching" {
    var five_n = Five{};
    const magic_num = MagicNumber.dynamicInit(&five_n);

    try testing.expect(magic_num.call("getMagicNumber", .{}) == 5);
}

test "dynamic dispatching inside array" {
    var five_n = Five{};
    var six_n = Six{};

    var list = std.ArrayList(MagicNumber.Impl(.Dyn)).init(testing.allocator);
    defer list.deinit();

    try list.append(MagicNumber.dynamicInit(&five_n));
    try list.append(MagicNumber.dynamicInit(&six_n));

    try testing.expect(list.items[0].call("getMagicNumber", .{}) == 5);
    try testing.expect(list.items[1].call("getMagicNumber", .{}) == 6);
}

test "check if is interface" {
    try testing.expect(MagicNumber.isBaseOf(MagicNumber.Impl(.Dyn)));
    try testing.expect(MagicNumber.isBaseOf(MagicNumber.Impl(Five)));
    try testing.expect(MagicNumber.isBaseOf(MagicNumber.Impl(Six)));
    try testing.expect(!MagicNumber.isBaseOf(i32));
}

test "function that takes interface as parameter (explicit impl kind)" {
    const local = struct {
        fn function(comptime impl_k: anytype, impl: MagicNumber.Impl(impl_k)) u8 {
            return impl.call("getMagicNumber", .{});
        }
    };

    {
        var five_n = Five{};
        const result = local.function(Five, MagicNumber.staticInit(&five_n));
        try testing.expect(result == 5);
    }

    {
        var six_n = Six{};
        const result = local.function(.Dyn, MagicNumber.dynamicInit(&six_n));
        try testing.expect(result == 6);
    }
}

test "function that takes interface as parameter (implicit impl kind)" {
    const local = struct {
        fn function(impl: anytype) u8 {
            if (comptime !MagicNumber.isBaseOf(@TypeOf(impl))) {
                @compileError("The passed argument is not an impl of MagicNumber.");
            }

            return impl.call("getMagicNumber", .{});
        }
    };

    {
        var five_n = Five{};
        const result = local.function(MagicNumber.staticInit(&five_n));
        try testing.expect(result == 5);
    }

    {
        var six_n = Six{};
        const result = local.function(MagicNumber.dynamicInit(&six_n));
        try testing.expect(result == 6);
    }
}
