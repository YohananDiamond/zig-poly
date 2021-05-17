//! The entry point of all the tests in this project.

test {
    _ = @import("basic.zig");
    _ = @import("multi-args.zig");
    _ = @import("decls.zig");
}
