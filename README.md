# zig-poly

Another attempt at a zig polymorphism library, heavily inspired (and
maybe partially stolen from) by
https://github.com/alexnask/interface.zig

Last tested on `zig-linux-x86_64-0.9.0-dev.749+259f3458a`

The main benefit over `interface.zig` is that the dispatching can be
done statically and dynamically, depending on the need. It's kind of
verbose, but it seems functional. Here's an adapted example from the tests:

```zig
const std = @import("std");
const testing = std.testing;
const poly = @import("/path/to/poly.zig");

// The interface declaration. Its vtable takes a single function,
// `getMagicNumber`, that is "generic" over the type of the implementer
// (SelfType).
const MagicNumber = poly.Interface(struct {
    getMagicNumber: fn (self: *poly.SelfType) u8,
}, .{});

const Five = struct {
    pub fn getMagicNumber(self: *@This()) u8 {
        return 5;
    }
};

const Six = struct {
    pub fn getMagicNumber(self: *@This()) u8 {
        return 6;
    }
};

// This function can take any kind of implementation for MagicNumber, be
// it static or dynamic, and work upon it.
fn function(comptime impl_k: anytype, impl: MagicNumber.Impl(impl_k)) u8 {
    return impl.call("getMagicNumber", .{});
}

test {
    var five_n = Five{};
    var six_n = Six{};

    // Static interface instantiation over the `Five` type
    {
        const result = function(Five, MagicNumber.staticInit(&five_n));
        try testing.expect(result == 5);
    }

    // Static interface instantiation over the `Six` type
    {
        const result = function(Six, MagicNumber.staticInit(&six_n));
        try testing.expect(result == 6);
    }

    // Dynamic interface instantation
    {
        // Notice that, instead of a type, we're passing `.Dyn` here.
        const result = function(.Dyn, MagicNumber.dynamicInit(&six_n));
        try testing.expect(result == 6);
    }
}
```

There's also early support for declaration dispatching. For more
in-depth examples, see the `src/tests` folder.

This is a work in progress and there still are features that haven't
been implemented (e.g. storage types other than "mutable pointer") or
well-tested yet.
