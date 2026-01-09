const std = @import("std");

/// Set of primitive type and value names.
/// Does not include `_` or logic and bit type names.
pub const names = std.StaticStringMap(void).initComptime(.{
    .{"anyclock"},
    .{"anyreset"},
    .{"comptime_int"},
});

/// Returns true if a name matches a primitive type or value, excluding `_`.
/// Logic and bit type names like `logic8` or `sbit32` are only matched for syntax,
/// so this will still return true when they have an oversized bit count
/// or leading zeroes.
pub fn isPrimitive(name: []const u8) bool {
    if (names.get(name) != null) return true;
    if (name.len < 3) return false;
    if (std.mem.startsWith(u8, name, "bit")) {
        if (name.len > 3) {
            for (name[3..]) |c| switch (c) {
                '0'...'9' => {},
                else => return false,
            };
        }
        return true;
    }
    if (name.len < 4) return false;
    if (std.mem.startsWith(u8, name, "sbit")) {
        if (name.len > 4) {
            for (name[4..]) |c| switch (c) {
                '0'...'9' => {},
                else => return false,
            };
        }
        return true;
    }
    if (name.len < 5) return false;
    if (std.mem.startsWith(u8, name, "logic")) {
        if (name.len > 5) {
            for (name[5..]) |c| switch (c) {
                '0'...'9' => {},
                else => return false,
            };
        }
        return true;
    }
    if (name.len < 6) return false;
    if (std.mem.startsWith(u8, name, "slogic")) {
        if (name.len > 6) {
            for (name[6..]) |c| switch (c) {
                '0'...'9' => {},
                else => return false,
            };
        }
        return true;
    }
    return false;
}

test isPrimitive {
    const expect = std.testing.expect;
    try expect(!isPrimitive(""));
    try expect(!isPrimitive("_"));
    try expect(!isPrimitive("haberdasher"));
    try expect(!isPrimitive("bit100s"));
    try expect(isPrimitive("anyclock"));
    try expect(isPrimitive("comptime_int"));
    try expect(isPrimitive("logic"));
    try expect(isPrimitive("slogic10000"));
    try expect(isPrimitive("sbit999999999999999999"));
}
