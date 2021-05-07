const std = @import("std");
const testing = std.testing;

const poly = @import("../poly.zig");

const SelfReferencing = poly.Interface(struct {
    isSame: fn (self: *poly.SelfType, rhs: *poly.SelfType) bool, // FIXME: this is incompatible with the impl, yet it does not error out
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

// test "self-referencing method (dynamically dispatched)" { // TODO: make this a compile error - SelfType as a non-self function argument in dynamic dispatching
//     var st1 = St{};
//     var st2 = St{};
//     const iface = SelfReferencing.Impl(.Dyn).init(&st1);

//     testing.expect(iface.call("isSame", .{&st1}));
//     testing.expect(!iface.call("isSame", .{&st2}));
// }
