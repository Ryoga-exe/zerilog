const std = @import("std");

/// Set of primitive type and value names.
/// Does not include `_` or logic and bit type names.
pub const names = std.StaticStringMap(void).initComptime(.{
    .{"anyclock"},
    .{"anyreset"},
    .{"comptime_int"},
});

pub const Signedness = enum { unsigned, signed };

pub const LogicKind = enum { bit, logic };

pub const LogicInfo = struct {
    kind: LogicKind,
    signedness: Signedness,
    bits: u32,
};

/// Returns true if a name matches a primitive type or value, excluding `_`.
/// Logic and bit type names like `logic8` or `sbit32` are only matched for syntax,
/// so this will still return true when they have an oversized bit count
/// or leading zeroes.
pub fn isPrimitive(name: []const u8) bool {
    if (names.get(name) != null) return true;
    return isLogicBitsSyntax(name);
}

pub fn parseLogicBits(name: []const u8) ?LogicInfo {
    return parseLogicBitsPrefix(name, "logic", .logic, .unsigned) orelse
        parseLogicBitsPrefix(name, "slogic", .logic, .signed) orelse
        parseLogicBitsPrefix(name, "bit", .bit, .unsigned) orelse
        parseLogicBitsPrefix(name, "sbit", .bit, .signed);
}

pub fn isLogicBitsSyntax(name: []const u8) bool {
    return isLogicBitsSyntaxPrefix(name, "logic") or
        isLogicBitsSyntaxPrefix(name, "slogic") or
        isLogicBitsSyntaxPrefix(name, "bit") or
        isLogicBitsSyntaxPrefix(name, "sbit");
}

fn parseLogicBitsPrefix(name: []const u8, prefix: []const u8, kind: LogicKind, signedness: Signedness) ?LogicInfo {
    if (!std.mem.startsWith(u8, name, prefix)) return null;
    const digits = name[prefix.len..];
    if (digits.len == 0) {
        return .{ .kind = kind, .signedness = signedness, .bits = 1 };
    }
    for (digits) |ch| {
        if (!std.ascii.isDigit(ch)) return null;
    }
    const bits = std.fmt.parseUnsigned(u32, digits, 10) catch return null;
    if (bits == 0) return null;
    return .{ .kind = kind, .signedness = signedness, .bits = bits };
}

fn isLogicBitsSyntaxPrefix(name: []const u8, prefix: []const u8) bool {
    if (!std.mem.startsWith(u8, name, prefix)) return false;
    const digits = name[prefix.len..];
    if (digits.len == 0) return true;
    for (digits) |ch| {
        if (!std.ascii.isDigit(ch)) return false;
    }
    return true;
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
