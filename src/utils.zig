const std = @import("std");
const Tag = std.meta.Tag;
const TagPayload = std.meta.TagPayload;
const testing = std.testing;

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

pub fn identQuote(comptime name: []const u8) []const u8 {
    comptime {
    var should_quote: bool = false;

        if (name.len > 0) switch (name[0]) {
            'a'...'z', 'A'...'Z' => {},
            else => should_quote = true,
        };

        for (name[1..]) |c| {
            switch (c) {
                'a'...'z' => {},
                'A'...'Z' => {},
                '0'...'9' => {},
                '_' => {},
                else => should_quote = false,
            }
        }

        return if (should_quote) blk: {
            // TODO: escape special characters n' stuff
            break :blk "@\"" ++ name ++ "\"";
        } else name;
    }
}

test "quoting" {
    try testing.expect(std.mem.eql(u8, identQuote("helloWorld"), "helloWorld"));
}
