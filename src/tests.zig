const std = @import("std");
const testing = std.testing;

const poly = @import("polymorphism.zig");

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

    // testing.expect(magic_num.call("getMagicNumber", .{}) == 5);
}

test "simple dynamic dispatching" {
    var five_n = Five{};
    const magic_num = MagicNumber.dynamicInit(&five_n);

    // testing.expect(magic_num.call("getMagicNumber", .{}) == 5);
}

test "dynamic dispatching inside array" {
    var five_n = Five{};
    var six_n = Six{};

    var list = std.ArrayList(MagicNumber.Impl(.Dyn)).init(testing.allocator);
    defer list.deinit();

    try list.append(MagicNumber.dynamicInit(&five_n));
    try list.append(MagicNumber.dynamicInit(&six_n));

    // testing.expect(list.items[0].call("getMagicNumber", .{}) == 5);
    // testing.expect(list.items[0].call("getMagicNumber", .{}) == 6);
}
