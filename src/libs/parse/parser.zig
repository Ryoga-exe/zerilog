const std = @import("std");
const Ast = @import("Ast.zig");
const tokenizer = @import("tokenizer.zig");

const Token = tokenizer.Token;
const Tokenizer = tokenizer.Tokenizer;

pub fn parse(allocator: std.mem.Allocator, source: [:0]const u8) !Ast.Ast {
    var tokens = Ast.TokenList{};
    defer tokens.deinit(allocator);

    var tokenizer_state = Tokenizer.init(source);
    while (true) {
        const tok = tokenizer_state.next();
        try tokens.append(allocator, .{
            .tag = tok.tag,
            .start = @intCast(tok.loc.start),
        });
        if (tok.tag == .eof) break;
    }

    var parser = Parser.init(allocator, source, tokens.slice());
    const root = try parser.addNode(.root, 0, .{ .root = .{ .decls = 0 } });
    const root_list = try parser.parseRootList();
    parser.nodes.set(@intFromEnum(root), .{
        .tag = .root,
        .main_token = 0,
        .data = .{ .root = .{ .decls = root_list } },
    });

    var nodes = parser.nodes;
    parser.nodes = std.MultiArrayList(Ast.Node).empty;

    return Ast.Ast{
        .source = source,
        .tokens = tokens.toOwnedSlice(),
        .nodes = nodes.toOwnedSlice(),
        .extra_data = try parser.extra_data.toOwnedSlice(allocator),
        .errors = try parser.errors.toOwnedSlice(allocator),
        .root = root,
    };
}

const Parser = struct {
    allocator: std.mem.Allocator,
    source: [:0]const u8,
    tokens: Ast.TokenList.Slice,
    token_index: usize,
    nodes: std.MultiArrayList(Ast.Node),
    extra_data: std.ArrayList(u32),
    errors: std.ArrayList(Ast.Error),

    fn init(allocator: std.mem.Allocator, source: [:0]const u8, tokens: Ast.TokenList.Slice) Parser {
        var parser = Parser{
            .allocator = allocator,
            .source = source,
            .tokens = tokens,
            .token_index = 0,
            .nodes = std.MultiArrayList(Ast.Node).empty,
            .extra_data = std.ArrayList(u32).empty,
            .errors = std.ArrayList(Ast.Error).empty,
        };
        parser.skipTrivia();
        return parser;
    }

    fn currentTag(self: *const Parser) Token.Tag {
        return self.tokens.items(.tag)[self.token_index];
    }

    fn currentTokenIndex(self: *const Parser) Ast.TokenIndex {
        return @intCast(self.token_index);
    }

    fn advance(self: *Parser) void {
        if (self.token_index + 1 < self.tokens.len) {
            self.token_index += 1;
        }
        self.skipTrivia();
    }

    fn skipTrivia(self: *Parser) void {
        while (self.token_index < self.tokens.len) {
            switch (self.currentTag()) {
                .doc_comment, .container_doc_comment => self.token_index += 1,
                else => return,
            }
        }
    }

    fn addError(self: *Parser, token_index: usize, tag: Ast.Error.Tag, extra: Ast.Error.Extra) !void {
        try self.errors.append(self.allocator, .{
            .tag = tag,
            .token = @intCast(token_index),
            .extra = extra,
        });
    }

    fn addErrorExpectedToken(self: *Parser, token_index: usize, expected: Token.Tag, token_is_prev: bool) !void {
        try self.errors.append(self.allocator, .{
            .tag = .expected_token,
            .token = @intCast(token_index),
            .token_is_prev = token_is_prev,
            .extra = .{ .expected_tag = expected },
        });
    }

    fn addNode(self: *Parser, tag: Ast.Node.Tag, main_token: Ast.TokenIndex, data: Ast.Node.Data) !Ast.Node.Index {
        const idx = self.nodes.len;
        try self.nodes.append(self.allocator, .{
            .tag = tag,
            .main_token = main_token,
            .data = data,
        });
        return @enumFromInt(@as(u32, @intCast(idx)));
    }

    fn addList(self: *Parser, items: []const u32) !u32 {
        const start = self.extra_data.items.len;
        try self.extra_data.append(self.allocator, @intCast(items.len));
        for (items) |item| {
            try self.extra_data.append(self.allocator, item);
        }
        return @intCast(start);
    }

    fn expect(self: *Parser, tag: Token.Tag) !Ast.TokenIndex {
        const idx = self.token_index;
        if (self.currentTag() == tag) {
            self.advance();
            return @intCast(idx);
        }
        try self.addErrorExpectedToken(idx, tag, false);
        if (self.currentTag() != .eof) self.advance();
        return @intCast(idx);
    }

    fn syncToTopLevel(self: *Parser) void {
        while (true) {
            switch (self.currentTag()) {
                .keyword_const, .keyword_pub, .keyword_module, .eof => return,
                .semicolon => {
                    self.advance();
                    return;
                },
                else => self.advance(),
            }
        }
    }

    fn syncToStatementEnd(self: *Parser) void {
        while (true) {
            switch (self.currentTag()) {
                .semicolon => {
                    self.advance();
                    return;
                },
                .r_brace, .eof => return,
                else => self.advance(),
            }
        }
    }

    fn match(self: *Parser, tag: Token.Tag) bool {
        if (self.currentTag() == tag) {
            self.advance();
            return true;
        }
        return false;
    }

    fn parseRootList(self: *Parser) !u32 {
        var items = std.ArrayList(u32).empty;
        defer items.deinit(self.allocator);

        while (self.currentTag() != .eof) {
            const decl = try self.parseTopLevelDecl();
            try items.append(self.allocator, @intFromEnum(decl));
        }

        return self.addList(items.items);
    }

    fn parseTopLevelDecl(self: *Parser) !Ast.Node.Index {
        if (self.currentTag() == .keyword_comptime) {
            const comptime_tok = try self.expect(.keyword_comptime);
            const inner = try self.parseTopLevelDecl();
            return self.addNode(.@"comptime", comptime_tok, .{ .unary = inner });
        }

        return switch (self.currentTag()) {
            .keyword_const => self.parseConstDecl(),
            .keyword_pub => blk: {
                _ = try self.expect(.keyword_pub);
                if (self.currentTag() != .keyword_module) {
                    try self.addError(self.token_index, .expected_module_after_pub, .{ .none = {} });
                    if (self.currentTag() != .eof) self.advance();
                    break :blk self.addNode(.@"error", self.currentTokenIndex(), .{ .none = {} });
                }
                break :blk self.parseModuleDecl(true);
            },
            .keyword_module => self.parseModuleDecl(false),
            else => blk: {
                try self.addError(self.token_index, .expected_top_level, .{ .none = {} });
                if (self.currentTag() != .eof) self.advance();
                break :blk self.addNode(.@"error", self.currentTokenIndex(), .{ .none = {} });
            },
        };
    }

    fn parseConstDecl(self: *Parser) !Ast.Node.Index {
        const const_tok = try self.expect(.keyword_const);
        const name_tok = try self.expect(.identifier);
        _ = try self.expect(.equal);
        const value_expr = try self.parseExpr();
        if (!self.match(.semicolon)) {
            try self.addErrorExpectedToken(self.token_index, .semicolon, false);
            self.syncToTopLevel();
        }
        return self.addNode(.const_decl, const_tok, .{ .const_decl = .{ .name = name_tok, .value = value_expr } });
    }

    fn parseVarDecl(self: *Parser) !Ast.Node.Index {
        const var_tok = try self.expect(.keyword_var);
        const name_tok = try self.expect(.identifier);
        _ = try self.expect(.colon);
        const ty_expr = try self.parseExpr();
        if (!self.match(.semicolon)) {
            try self.addErrorExpectedToken(self.token_index, .semicolon, false);
            self.syncToStatementEnd();
        }
        return self.addNode(.var_decl, var_tok, .{ .var_decl = .{ .name = name_tok, .ty = ty_expr } });
    }

    fn parseModuleDecl(self: *Parser, is_pub: bool) !Ast.Node.Index {
        _ = try self.expect(.keyword_module);
        const name_tok = try self.expect(.identifier);
        _ = try self.expect(.l_paren);

        var ports = std.ArrayList(u32).empty;
        defer ports.deinit(self.allocator);

        if (self.currentTag() != .r_paren) {
            while (true) {
                const port = try self.parsePort();
                try ports.append(self.allocator, @intFromEnum(port));
                if (self.match(.comma)) {
                    if (self.currentTag() == .r_paren) break;
                    continue;
                }
                break;
            }
        }

        _ = try self.expect(.r_paren);

        const ports_list = try self.addList(ports.items);
        const body_block = try self.parseBlock();

        return self.addNode(if (is_pub) .pub_module_decl else .module_decl, name_tok, .{ .module_decl = .{ .ports = ports_list, .body = body_block } });
    }

    fn parsePort(self: *Parser) !Ast.Node.Index {
        const name_tok = try self.expect(.identifier);
        _ = try self.expect(.colon);

        const dir_tag = self.currentTag();
        switch (dir_tag) {
            .keyword_input, .keyword_output, .keyword_inout => self.advance(),
            else => {
                try self.addError(self.token_index, .expected_direction, .{ .none = {} });
                if (self.currentTag() != .eof) self.advance();
            },
        }

        const ty_expr = try self.parseExpr();
        return self.addNode(.port, name_tok, .{ .port = .{ .dir = @as(u32, @intFromEnum(dir_tag)), .ty = ty_expr } });
    }

    fn parseBlock(self: *Parser) std.mem.Allocator.Error!Ast.Node.Index {
        const l_brace = try self.expect(.l_brace);

        var statements = std.ArrayList(u32).empty;
        defer statements.deinit(self.allocator);

        while (self.currentTag() != .r_brace and self.currentTag() != .eof) {
            const stmt = try self.parseStatement();
            try statements.append(self.allocator, @intFromEnum(stmt));
        }

        _ = try self.expect(.r_brace);
        const list_index = try self.addList(statements.items);
        return self.addNode(.block, l_brace, .{ .block = .{ .statements = list_index } });
    }

    fn parseStatement(self: *Parser) std.mem.Allocator.Error!Ast.Node.Index {
        if (self.currentTag() == .keyword_comptime) {
            const comptime_tok = try self.expect(.keyword_comptime);
            const inner = try self.parseStatement();
            return self.addNode(.@"comptime", comptime_tok, .{ .unary = inner });
        }

        return switch (self.currentTag()) {
            .keyword_var => self.parseVarDecl(),
            .keyword_const => self.parseConstDecl(),
            .keyword_always_ff => self.parseAlwaysFF(),
            .keyword_comb => self.parseComb(),
            .keyword_if => self.parseIf(),
            .keyword_if_reset => self.parseIfReset(),
            .l_brace => self.parseBlock(),
            .identifier => self.parseAssign(),
            else => blk: {
                try self.addError(self.token_index, .expected_statement, .{ .none = {} });
                if (self.currentTag() != .eof) self.advance();
                break :blk self.addNode(.@"error", self.currentTokenIndex(), .{ .none = {} });
            },
        };
    }

    fn parseAlwaysFF(self: *Parser) !Ast.Node.Index {
        const tok = try self.expect(.keyword_always_ff);
        const block = try self.parseBlock();
        return self.addNode(.always_ff, tok, .{ .unary = block });
    }

    fn parseComb(self: *Parser) !Ast.Node.Index {
        const tok = try self.expect(.keyword_comb);
        const block = try self.parseBlock();
        return self.addNode(.comb, tok, .{ .unary = block });
    }

    fn parseIf(self: *Parser) !Ast.Node.Index {
        const tok = try self.expect(.keyword_if);
        const cond = try self.parseExpr();
        const then_block = try self.parseBlock();

        var else_block: Ast.Node.OptionalIndex = .none;
        if (self.match(.keyword_else)) {
            const block = try self.parseBlock();
            else_block = block.toOptional();
        }

        return self.addNode(.if_stmt, tok, .{ .if_stmt = .{ .cond = cond, .then_block = then_block, .else_block = else_block } });
    }

    fn parseIfReset(self: *Parser) !Ast.Node.Index {
        const tok = try self.expect(.keyword_if_reset);
        const then_block = try self.parseBlock();

        var else_block: Ast.Node.OptionalIndex = .none;
        if (self.match(.keyword_else)) {
            const block = try self.parseBlock();
            else_block = block.toOptional();
        }

        return self.addNode(.if_reset, tok, .{ .if_reset = .{ .then_block = then_block, .else_block = else_block } });
    }

    fn parseAssign(self: *Parser) !Ast.Node.Index {
        const name_tok = try self.expect(.identifier);
        const target = try self.addNode(.identifier, name_tok, .{ .none = {} });

        const op_tok = self.currentTokenIndex();
        const op_tag = self.currentTag();
        switch (op_tag) {
            .equal, .plus_equal => self.advance(),
            else => {
                try self.addError(self.token_index, .expected_assignment_op, .{ .none = {} });
                if (self.currentTag() != .eof) self.advance();
            },
        }

        const rhs = try self.parseExpr();
        if (!self.match(.semicolon)) {
            try self.addErrorExpectedToken(self.token_index, .semicolon, false);
            self.syncToStatementEnd();
        }

        return self.addNode(.assign, op_tok, .{ .assign = .{ .target = target, .value = rhs } });
    }

    fn parseExpr(self: *Parser) std.mem.Allocator.Error!Ast.Node.Index {
        return self.parseBinaryExpr(1);
    }

    fn parseBinaryExpr(self: *Parser, min_prec: u8) std.mem.Allocator.Error!Ast.Node.Index {
        var lhs = try self.parsePrimary();
        while (true) {
            const op_tag = self.currentTag();
            const prec = precedence(op_tag);
            if (prec < min_prec) break;

            const op_tok = self.currentTokenIndex();
            self.advance();
            const rhs = try self.parseBinaryExpr(prec + 1);
            lhs = try self.addNode(.binary, op_tok, .{ .binary = .{ .lhs = lhs, .rhs = rhs } });
        }
        return lhs;
    }

    fn parsePrimary(self: *Parser) std.mem.Allocator.Error!Ast.Node.Index {
        switch (self.currentTag()) {
            .identifier => {
                const tok = self.currentTokenIndex();
                self.advance();
                const node = try self.addNode(.identifier, tok, .{ .none = {} });
                return self.parsePostfix(node);
            },
            .number_literal => {
                const tok = self.currentTokenIndex();
                self.advance();
                return self.addNode(.number_literal, tok, .{ .none = {} });
            },
            .builtin => {
                const tok = self.currentTokenIndex();
                self.advance();
                const node = try self.addNode(.builtin_ref, tok, .{ .none = {} });
                return self.parsePostfix(node);
            },
            .period => {
                self.advance();
                const name_tok = try self.expect(.identifier);
                const node = try self.addNode(.enum_literal, name_tok, .{ .none = {} });
                return self.parsePostfix(node);
            },
            .l_paren => {
                _ = try self.expect(.l_paren);
                const expr = try self.parseExpr();
                _ = try self.expect(.r_paren);
                return expr;
            },
            .keyword_comptime => {
                const comptime_tok = try self.expect(.keyword_comptime);
                const inner = try self.parsePrimary();
                return self.addNode(.@"comptime", comptime_tok, .{ .unary = inner });
            },
            else => {
                try self.addError(self.token_index, .expected_expression, .{ .none = {} });
                if (self.currentTag() != .eof) self.advance();
                return self.addNode(.@"error", self.currentTokenIndex(), .{ .none = {} });
            },
        }
    }

    fn parsePostfix(self: *Parser, base: Ast.Node.Index) std.mem.Allocator.Error!Ast.Node.Index {
        var expr = base;
        while (true) {
            switch (self.currentTag()) {
                .l_paren => {
                    const l_paren = try self.expect(.l_paren);
                    var args = std.ArrayList(u32).empty;
                    defer args.deinit(self.allocator);

                    if (self.currentTag() != .r_paren) {
                        while (true) {
                            const arg = try self.parseExpr();
                            try args.append(self.allocator, @intFromEnum(arg));
                            if (self.match(.comma)) {
                                if (self.currentTag() == .r_paren) break;
                                continue;
                            }
                            break;
                        }
                    }

                    _ = try self.expect(.r_paren);
                    const args_list = try self.addList(args.items);
                    expr = try self.addNode(.call, l_paren, .{ .call = .{ .callee = expr, .args = args_list } });
                },
                .period => {
                    const period_tok = try self.expect(.period);
                    const name_tok = try self.expect(.identifier);
                    expr = try self.addNode(.field_access, period_tok, .{ .field_access = .{ .lhs = expr, .field = name_tok } });
                },
                else => return expr,
            }
        }
    }
};

fn precedence(tag: Token.Tag) u8 {
    return switch (tag) {
        .equal_equal => 1,
        .plus, .minus => 2,
        else => 0,
    };
}

test "parse counter module" {
    const source =
        \\const Clock = clock_posedge;
        \\const Reset = reset_async_low;
        \\
        \\pub module Counter(
        \\    clk: input Clock,
        \\    rst: input Reset,
        \\) {
        \\    var r: logic8;
        \\
        \\    always_ff {
        \\        if_reset {
        \\            r = 0;
        \\        } else {
        \\            r = r;
        \\        }
        \\    }
        \\}
    ;

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var source_buf = try allocator.alloc(u8, source.len + 1);
    defer allocator.free(source_buf);
    std.mem.copyForwards(u8, source_buf[0..source.len], source);
    source_buf[source.len] = 0;

    const ast = try parse(allocator, source_buf[0..source.len :0]);
    defer {
        var ast_mut = ast;
        ast_mut.deinit(allocator);
    }

    try std.testing.expectEqual(@as(usize, 0), ast.errors.len);
    try std.testing.expect(ast.nodes.len > 0);
}
