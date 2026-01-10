const std = @import("std");
const tokenizer = @import("tokenizer.zig");

pub const Token = tokenizer.Token;
const Tokenizer = tokenizer.Tokenizer;

pub const ByteOffset = u32;

pub const TokenList = std.MultiArrayList(struct {
    tag: Token.Tag,
    start: ByteOffset,
});

pub const NodeList = std.MultiArrayList(Node);

pub const TokenIndex = u32;

pub const OptionalTokenIndex = enum(u32) {
    none = std.math.maxInt(u32),
    _,

    pub fn unwrap(oti: OptionalTokenIndex) ?TokenIndex {
        return if (oti == .none) null else @intFromEnum(oti);
    }

    pub fn fromToken(ti: TokenIndex) OptionalTokenIndex {
        return @enumFromInt(ti);
    }

    pub fn fromOptional(oti: ?TokenIndex) OptionalTokenIndex {
        return if (oti) |ti| @enumFromInt(ti) else .none;
    }
};

pub const TokenOffset = enum(i32) {
    zero = 0,
    _,

    pub fn init(base: TokenIndex, destination: TokenIndex) TokenOffset {
        const base_i64: i64 = base;
        const destination_i64: i64 = destination;
        return @enumFromInt(destination_i64 - base_i64);
    }

    pub fn toOptional(to: TokenOffset) OptionalTokenOffset {
        const result: OptionalTokenOffset = @enumFromInt(@intFromEnum(to));
        std.debug.assert(result != .none);
        return result;
    }

    pub fn toAbsolute(offset: TokenOffset, base: TokenIndex) TokenIndex {
        return @intCast(@as(i64, base) + @intFromEnum(offset));
    }
};

pub const OptionalTokenOffset = enum(i32) {
    none = std.math.maxInt(i32),
    _,

    pub fn unwrap(oto: OptionalTokenOffset) ?TokenOffset {
        return if (oto == .none) null else @enumFromInt(@intFromEnum(oto));
    }
};

pub const Error = struct {
    tag: Tag,
    is_note: bool = false,
    token_is_prev: bool = false,
    token: TokenIndex,
    extra: Extra = .{ .none = {} },

    pub const Extra = union {
        none: void,
        expected_tag: Token.Tag,
        offset: usize,
    };

    pub const Tag = enum {
        expected_token,
        expected_top_level,
        expected_statement,
        expected_expression,
        expected_direction,
        expected_assignment_op,
        expected_module_after_pub,
    };
};

pub const Location = struct {
    line: usize,
    column: usize,
    line_start: usize,
    line_end: usize,
};

pub const Ast = struct {
    source: [:0]const u8,
    tokens: TokenList.Slice,
    nodes: NodeList.Slice,
    extra_data: []u32,
    errors: []const Error,
    root: Node.Index,

    pub fn deinit(self: *Ast, allocator: std.mem.Allocator) void {
        self.tokens.deinit(allocator);
        self.nodes.deinit(allocator);
        allocator.free(self.extra_data);
        allocator.free(self.errors);
        self.* = undefined;
    }

    pub fn tokenTag(self: *const Ast, token_index: TokenIndex) Token.Tag {
        return self.tokens.items(.tag)[token_index];
    }

    pub fn tokenStart(self: *const Ast, token_index: TokenIndex) ByteOffset {
        return self.tokens.items(.start)[token_index];
    }

    pub fn nodeTag(self: *const Ast, node: Node.Index) Node.Tag {
        return self.nodes.items(.tag)[@intFromEnum(node)];
    }

    pub fn nodeMainToken(self: *const Ast, node: Node.Index) TokenIndex {
        return self.nodes.items(.main_token)[@intFromEnum(node)];
    }

    pub fn nodeData(self: *const Ast, node: Node.Index) Node.Data {
        return self.nodes.items(.data)[@intFromEnum(node)];
    }

    pub fn tokenSlice(self: *const Ast, token_index: TokenIndex) []const u8 {
        const token_tag = self.tokenTag(token_index);
        if (token_tag.lexeme()) |lexeme| {
            return lexeme;
        }
        var tokenizer_state = Tokenizer.init(self.source);
        tokenizer_state.index = self.tokenStart(token_index);
        const token = tokenizer_state.next();
        std.debug.assert(token.tag == token_tag);
        return self.source[token.loc.start..token.loc.end];
    }

    pub fn listSlice(self: *const Ast, list_index: u32) []const u32 {
        const idx: usize = @intCast(list_index);
        const len: usize = @intCast(self.extra_data[idx]);
        const start = idx + 1;
        return self.extra_data[start .. start + len];
    }

    pub fn tokenLocation(self: Ast, token_index: TokenIndex) Location {
        var loc = Location{
            .line = 1,
            .column = 1,
            .line_start = 0,
            .line_end = self.source.len,
        };
        const token_start = self.tokenStart(token_index);

        var i: usize = 0;
        while (i < self.source.len) : (i += 1) {
            const c = self.source[i];
            if (i == token_start) {
                if (std.mem.indexOfScalarPos(u8, self.source, i, '\n')) |end| {
                    loc.line_end = end;
                }
                return loc;
            }
            if (c == '\n') {
                loc.line += 1;
                loc.column = 1;
                loc.line_start = i + 1;
            } else {
                loc.column += 1;
            }
        }
        return loc;
    }
};

pub const Node = struct {
    tag: Tag,
    main_token: TokenIndex,
    data: Data,

    pub const Index = enum(u32) {
        root = 0,
        _,

        pub fn toOptional(i: Index) OptionalIndex {
            const result: OptionalIndex = @enumFromInt(@intFromEnum(i));
            std.debug.assert(result != .none);
            return result;
        }

        pub fn toOffset(base: Index, destination: Index) Offset {
            const base_i64: i64 = @intFromEnum(base);
            const destination_i64: i64 = @intFromEnum(destination);
            return @enumFromInt(destination_i64 - base_i64);
        }
    };

    pub const OptionalIndex = enum(u32) {
        root = 0,
        none = std.math.maxInt(u32),
        _,

        pub fn unwrap(oi: OptionalIndex) ?Index {
            return if (oi == .none) null else @enumFromInt(@intFromEnum(oi));
        }

        pub fn fromOptional(oi: ?Index) OptionalIndex {
            return if (oi) |i| i.toOptional() else .none;
        }
    };

    pub const Offset = enum(i32) {
        zero = 0,
        _,

        pub fn toOptional(o: Offset) OptionalOffset {
            const result: OptionalOffset = @enumFromInt(@intFromEnum(o));
            std.debug.assert(result != .none);
            return result;
        }

        pub fn toAbsolute(offset: Offset, base: Index) Index {
            return @enumFromInt(@as(i64, @intFromEnum(base)) + @intFromEnum(offset));
        }
    };

    pub const OptionalOffset = enum(i32) {
        none = std.math.maxInt(i32),
        _,

        pub fn unwrap(o: OptionalOffset) ?Offset {
            return if (o == .none) null else @enumFromInt(@intFromEnum(o));
        }
    };

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

pub const null_node: Node.Index = @enumFromInt(std.math.maxInt(u32));
pub const null_node_index: u32 = std.math.maxInt(u32);
