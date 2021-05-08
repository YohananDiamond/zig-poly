const std = @import("std");
const testing = std.testing;

const poly = @import("../poly.zig");

const SelfReferencing = poly.Interface(struct {
    isSame: fn (self: *poly.SelfType, rhs: *poly.SelfType) bool,
}, .{});

const St = struct {
    _dummy: i8 = 0,

    const Self = @This();

    pub fn isSame(self: *Self, rhs: *Self) bool {
        return @ptrToInt(self) == @ptrToInt(rhs);
    }
};

test "self-referencing method" {
    var st1 = St{};
    var st2 = St{};
    const iface = SelfReferencing.Impl(St).init(&st1);

    testing.expect(iface.call("isSame", .{&st1}));
    testing.expect(!iface.call("isSame", .{&st2}));
}

// Works as expected (compile error message), but can't let it here or the other tests won't run.
// test "self-referencing method (dynamically dispatched)" {
//     var st1 = St{};
//     var st2 = St{};
//     const iface = SelfReferencing.Impl(.Dyn).init(&st1);

//     testing.expect(iface.call("isSame", .{&st1}));
//     testing.expect(!iface.call("isSame", .{&st2}));
// }
