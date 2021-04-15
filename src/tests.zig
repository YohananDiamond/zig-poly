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

test "static dispatching" {
    var five_n = Five{};
    const magic_num = MagicNumber.staticInit(&five_n);

    // testing.expect(magic_num.call("getMagicNumber", .{}) == 5);
}
