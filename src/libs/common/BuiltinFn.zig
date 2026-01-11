const std = @import("std");

pub const Tag = enum {
    logic,
    bit,
};

pub const Kind = enum {
    type_ctor,
    value,
};

pub const Info = struct {
    tag: Tag,
    kind: Kind,
    param_count: ?u8,
};

pub const list = list: {
    break :list std.StaticStringMap(Info).initComptime([_]struct { []const u8, Info }{
        .{
            "@Logic",
            .{
                .tag = .logic,
                .kind = .type_ctor,
                .param_count = 2,
            },
        },
        .{
            "@Bit",
            .{
                .tag = .bit,
                .kind = .type_ctor,
                .param_count = 2,
            },
        },
    });
};

pub fn get(name: []const u8) ?Info {
    return list.get(name);
}
