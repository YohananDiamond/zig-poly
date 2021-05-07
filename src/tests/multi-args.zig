const std = @import("std");
const testing = std.testing;

const poly = @import("../poly.zig");

const SelfReferencing = poly.Interface(struct {
    isSame: fn (self: *poly.SelfType, rhs: *poly.SelfType, _a: u32, _b: *poly.SelfType) bool, // FIXME: this is incompatible with the impl, yet it does not error out
}, .{});

const St = struct {
    _dummy: i8 = 0,

    const Self = @This();

    pub fn isSame(self: *Self, rhs: *Self, _a: u32, _b: *Self) bool {
        return @ptrToInt(self) == @ptrToInt(rhs);
    }
};

test "self-referencing method" {
    var st1 = St{};
    var st2 = St{};
    const iface = SelfReferencing.Impl(St).init(&st1);

    testing.expect(iface.call("isSame", .{&st1, 10, &st2}));
    testing.expect(!iface.call("isSame", .{&st2, 10, &st1}));
}
