const std = @import("std");
const Tag = std.meta.Tag;
const TagPayload = std.meta.TagPayload;

/// Format a string at compile-time and then throw a compile-error with its contents.
pub fn fmtError(comptime fmt: []const u8, comptime args: anytype) noreturn {
    @compileError(std.fmt.comptimePrint(fmt, args));
}

/// Get a specific variant of an union, or return null.
///
/// Inspired from https://github.com/ziglang/zig/issues/8109#issuecomment-787959353
///
/// TODO: how to caputre by pointer with this?
pub fn unwrapUnion(un: anytype, comptime tag: Tag(@TypeOf(un))) ?TagPayload(@TypeOf(un), tag) {
    return if (un != tag) null else @field(un, @tagName(tag));
}
