//! Tests related to dispatching of declarations (instead of function).

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

    try testing.expect(iface.get("val1") == 16);
    try testing.expect(iface.get("val2") == 50);
}

test "dynamic dispatching + declarations" {
    var simple = SimpleImpl{};
    const iface = SimpleDeclFace.Impl(.Dyn).init(&simple);

    try testing.expect(iface.get("val1") == 16);
    try testing.expect(iface.get("val2") == 50);
}

const ComptimeOnlyFace = poly.Interface(struct {
    ctime_val: type,
}, .{});

const ComptimeImpl = struct {
    pub const ctime_val = i32;
};

// FIXME: this triggers an assertion failure - not sure how to test that
// test "comptime-only interface because of type declaration" {
//     var impl = ComptimeImpl{};
//     const iface = ComptimeOnlyFace.Impl(ComptimeImpl).init(&impl);

//     try testing.expect(iface.get("ctime_val") == i32);
// }
