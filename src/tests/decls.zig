//! Tests related to dispatching of declarations.

const std = @import("std");
const testing = std.testing;
const poly = @import("../poly.zig");

const SimpleDeclFace = poly.Interface(struct {
    val1: i32,
    val2: u32,
}, .{});

const SimpleImpl = struct {
    pub const val1: i32 = 16;
    pub const val2: u32 = 50;
};

test "static dispatching + declarations" {
    var simple = SimpleImpl{};
    const iface = SimpleDeclFace.Impl(SimpleImpl).init(&simple);

    testing.expect(iface.get("val1") == 16);
    testing.expect(iface.get("val2") == 50);
}

test "dynamic dispatching + declarations" {
    var simple = SimpleImpl{};
    const iface = SimpleDeclFace.Impl(.Dyn).init(&simple);

    testing.expect(iface.get("val1") == 16);
    testing.expect(iface.get("val2") == 50);
}

const ComptimeOnlyFace = poly.Interface(struct {
    ctime_val: type,
}, .{});

const ComptimeImpl = struct {
    pub const ctime_val = i32;
};

// test "comptime-only interface because of type declaration" { // FIXME: this triggers an assertion failure
//     var impl = ComptimeImpl{};
//     const iface = ComptimeOnlyFace.Impl(ComptimeImpl).init(&impl);

//     testing.expect(iface.get("ctime_val") == i32);
// }
