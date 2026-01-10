const std = @import("std");
const tokenizer = @import("tokenizer.zig");

pub const Token = tokenizer.Token;

pub const Ast = struct {
    source: [:0]const u8,
    tokens: []Token,
    nodes: []Node,
    extra_data: []Node.Index,
    errors: []Error,
    root: Node.Index,

    pub fn deinit(self: *Ast, allocator: std.mem.Allocator) void {
        allocator.free(self.tokens);
        allocator.free(self.nodes);
        allocator.free(self.extra_data);
        allocator.free(self.errors);
        self.* = undefined;
    }

    pub fn tokenSlice(self: *const Ast, token_index: TokenIndex) []const u8 {
        const token = self.tokens[@intCast(token_index)];
        return self.source[token.loc.start..token.loc.end];
    }

    pub fn listSlice(self: *const Ast, list_index: u32) []const Node.Index {
        const idx: usize = @intCast(list_index);
        const len: usize = @intCast(self.extra_data[idx]);
        const start = idx + 1;
        return self.extra_data[start .. start + len];
    }
};

pub const TokenIndex = u32;

pub const Error = struct {
    token: TokenIndex,
    message: []const u8,
};

pub const Node = struct {
    tag: Tag,
    main_token: TokenIndex,
    data: Data,

    pub const Index = u32;

    pub const Data = struct {
        lhs: u32,
        rhs: u32,
    };

    pub const Tag = enum {
        root,
        const_decl,
        var_decl,
        module_decl,
        pub_module_decl,
        port,
        block,
        always_ff,
        comb,
        if_stmt,
        if_reset,
        assign,
        binary,
        identifier,
        number_literal,
        builtin_ref,
        enum_literal,
        call,
        field_access,
        @"comptime",
        @"error",
    };
};

pub const null_node: Node.Index = std.math.maxInt(Node.Index);
