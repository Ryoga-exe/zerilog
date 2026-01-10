const std = @import("std");
const Ast = @import("Ast.zig");
const tokenizer = @import("tokenizer.zig");

const Token = tokenizer.Token;
const Tokenizer = tokenizer.Tokenizer;

pub fn parse(allocator: std.mem.Allocator, source: [:0]const u8) !Ast.Ast {
    var tokens_list = std.ArrayList(Token).empty;
    defer tokens_list.deinit(allocator);

    var tokenizer_state = Tokenizer.init(source);
    while (true) {
        const tok = tokenizer_state.next();
        try tokens_list.append(allocator, tok);
        if (tok.tag == .eof) break;
    }

    var parser = Parser.init(allocator, source, tokens_list.items);
    const root_list = try parser.parseRootList();
    const root = try parser.addNode(.root, 0, .{ .lhs = root_list, .rhs = 0 });

    return Ast.Ast{
        .source = source,
        .tokens = try tokens_list.toOwnedSlice(allocator),
        .nodes = try parser.nodes.toOwnedSlice(allocator),
        .extra_data = try parser.extra_data.toOwnedSlice(allocator),
        .errors = try parser.errors.toOwnedSlice(allocator),
        .root = root,
    };
}

const Parser = struct {
    allocator: std.mem.Allocator,
    source: [:0]const u8,
    tokens: []const Token,
    token_index: usize,
    nodes: std.ArrayList(Ast.Node),
    extra_data: std.ArrayList(Ast.Node.Index),
    errors: std.ArrayList(Ast.Error),

    fn init(allocator: std.mem.Allocator, source: [:0]const u8, tokens: []const Token) Parser {
        var parser = Parser{
            .allocator = allocator,
            .source = source,
            .tokens = tokens,
            .token_index = 0,
            .nodes = std.ArrayList(Ast.Node).empty,
            .extra_data = std.ArrayList(Ast.Node.Index).empty,
            .errors = std.ArrayList(Ast.Error).empty,
        };
        parser.skipTrivia();
        return parser;
    }

    fn currentTag(self: *const Parser) Token.Tag {
        return self.tokens[self.token_index].tag;
    }

    fn currentTokenIndex(self: *const Parser) Ast.TokenIndex {
        return @intCast(self.token_index);
    }

    fn advance(self: *Parser) void {
        if (self.token_index < self.tokens.len - 1) {
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

    fn addError(self: *Parser, token_index: usize, message: []const u8) !void {
        try self.errors.append(self.allocator, .{
            .token = @intCast(token_index),
            .message = message,
        });
    }

    fn addNode(self: *Parser, tag: Ast.Node.Tag, main_token: Ast.TokenIndex, data: Ast.Node.Data) !Ast.Node.Index {
        const idx = self.nodes.items.len;
        try self.nodes.append(self.allocator, .{
            .tag = tag,
            .main_token = main_token,
            .data = data,
        });
        return @intCast(idx);
    }

    fn addList(self: *Parser, items: []const Ast.Node.Index) !u32 {
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
        try self.addError(idx, tag.symbol());
        if (self.currentTag() != .eof) self.advance();
        return @intCast(idx);
    }

    fn match(self: *Parser, tag: Token.Tag) bool {
        if (self.currentTag() == tag) {
            self.advance();
            return true;
        }
        return false;
    }

    fn parseRootList(self: *Parser) !u32 {
        var items = std.ArrayList(Ast.Node.Index).empty;
        defer items.deinit(self.allocator);

        while (self.currentTag() != .eof) {
            const decl = try self.parseTopLevelDecl();
            if (decl != Ast.null_node) {
                try items.append(self.allocator, decl);
            }
        }

        return self.addList(items.items);
    }

    fn parseTopLevelDecl(self: *Parser) !Ast.Node.Index {
        if (self.currentTag() == .keyword_comptime) {
            const comptime_tok = try self.expect(.keyword_comptime);
            const inner = try self.parseTopLevelDecl();
            return self.addNode(.@"comptime", comptime_tok, .{ .lhs = inner, .rhs = 0 });
        }

        return switch (self.currentTag()) {
            .keyword_const => self.parseConstDecl(),
            .keyword_pub => blk: {
                _ = try self.expect(.keyword_pub);
                if (self.currentTag() != .keyword_module) {
                    try self.addError(self.token_index, "expected 'module' after 'pub'");
                    if (self.currentTag() != .eof) self.advance();
                    break :blk self.addNode(.@"error", self.currentTokenIndex(), .{ .lhs = 0, .rhs = 0 });
                }
                break :blk self.parseModuleDecl(true);
            },
            .keyword_module => self.parseModuleDecl(false),
            .eof => Ast.null_node,
            else => blk: {
                try self.addError(self.token_index, "expected top-level declaration");
                if (self.currentTag() != .eof) self.advance();
                break :blk self.addNode(.@"error", self.currentTokenIndex(), .{ .lhs = 0, .rhs = 0 });
            },
        };
    }

    fn parseConstDecl(self: *Parser) !Ast.Node.Index {
        const const_tok = try self.expect(.keyword_const);
        const name_tok = try self.expect(.identifier);
        _ = try self.expect(.equal);
        const value_expr = try self.parseExpr();
        _ = try self.expect(.semicolon);
        return self.addNode(.const_decl, const_tok, .{ .lhs = name_tok, .rhs = value_expr });
    }

    fn parseVarDecl(self: *Parser) !Ast.Node.Index {
        const var_tok = try self.expect(.keyword_var);
        const name_tok = try self.expect(.identifier);
        _ = try self.expect(.colon);
        const ty_expr = try self.parseExpr();
        _ = try self.expect(.semicolon);
        return self.addNode(.var_decl, var_tok, .{ .lhs = name_tok, .rhs = ty_expr });
    }

    fn parseModuleDecl(self: *Parser, is_pub: bool) !Ast.Node.Index {
        _ = try self.expect(.keyword_module);
        const name_tok = try self.expect(.identifier);
        _ = try self.expect(.l_paren);

        var ports = std.ArrayList(Ast.Node.Index).empty;
        defer ports.deinit(self.allocator);

        if (self.currentTag() != .r_paren) {
            while (true) {
                const port = try self.parsePort();
                try ports.append(self.allocator, port);
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

        return self.addNode(if (is_pub) .pub_module_decl else .module_decl, name_tok, .{ .lhs = ports_list, .rhs = body_block });
    }

    fn parsePort(self: *Parser) !Ast.Node.Index {
        const name_tok = try self.expect(.identifier);
        _ = try self.expect(.colon);

        const dir_tag = self.currentTag();
        switch (dir_tag) {
            .keyword_input, .keyword_output, .keyword_inout => self.advance(),
            else => {
                try self.addError(self.token_index, "expected port direction");
                if (self.currentTag() != .eof) self.advance();
            },
        }

        const ty_expr = try self.parseExpr();
        return self.addNode(.port, name_tok, .{ .lhs = @as(u32, @intFromEnum(dir_tag)), .rhs = ty_expr });
    }

    fn parseBlock(self: *Parser) std.mem.Allocator.Error!Ast.Node.Index {
        const l_brace = try self.expect(.l_brace);

        var statements = std.ArrayList(Ast.Node.Index).empty;
        defer statements.deinit(self.allocator);

        while (self.currentTag() != .r_brace and self.currentTag() != .eof) {
            const stmt = try self.parseStatement();
            if (stmt != Ast.null_node) {
                try statements.append(self.allocator, stmt);
            }
        }

        _ = try self.expect(.r_brace);
        const list_index = try self.addList(statements.items);
        return self.addNode(.block, l_brace, .{ .lhs = list_index, .rhs = 0 });
    }

    fn parseStatement(self: *Parser) std.mem.Allocator.Error!Ast.Node.Index {
        if (self.currentTag() == .keyword_comptime) {
            const comptime_tok = try self.expect(.keyword_comptime);
            const inner = try self.parseStatement();
            return self.addNode(.@"comptime", comptime_tok, .{ .lhs = inner, .rhs = 0 });
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
                try self.addError(self.token_index, "expected statement");
                if (self.currentTag() != .eof) self.advance();
                break :blk self.addNode(.@"error", self.currentTokenIndex(), .{ .lhs = 0, .rhs = 0 });
            },
        };
    }

    fn parseAlwaysFF(self: *Parser) !Ast.Node.Index {
        const tok = try self.expect(.keyword_always_ff);
        const block = try self.parseBlock();
        return self.addNode(.always_ff, tok, .{ .lhs = block, .rhs = 0 });
    }

    fn parseComb(self: *Parser) !Ast.Node.Index {
        const tok = try self.expect(.keyword_comb);
        const block = try self.parseBlock();
        return self.addNode(.comb, tok, .{ .lhs = block, .rhs = 0 });
    }

    fn parseIf(self: *Parser) !Ast.Node.Index {
        const tok = try self.expect(.keyword_if);
        const cond = try self.parseExpr();
        const then_block = try self.parseBlock();

        var else_block: Ast.Node.Index = Ast.null_node;
        if (self.match(.keyword_else)) {
            else_block = try self.parseBlock();
        }

        const list_index = try self.addList(&.{ then_block, else_block });
        return self.addNode(.if_stmt, tok, .{ .lhs = cond, .rhs = list_index });
    }

    fn parseIfReset(self: *Parser) !Ast.Node.Index {
        const tok = try self.expect(.keyword_if_reset);
        const then_block = try self.parseBlock();

        var else_block: Ast.Node.Index = Ast.null_node;
        if (self.match(.keyword_else)) {
            else_block = try self.parseBlock();
        }

        return self.addNode(.if_reset, tok, .{ .lhs = then_block, .rhs = else_block });
    }

    fn parseAssign(self: *Parser) !Ast.Node.Index {
        const name_tok = try self.expect(.identifier);
        const target = try self.addNode(.identifier, name_tok, .{ .lhs = 0, .rhs = 0 });

        const op_tok = self.currentTokenIndex();
        const op_tag = self.currentTag();
        switch (op_tag) {
            .equal, .plus_equal => self.advance(),
            else => {
                try self.addError(self.token_index, "expected assignment operator");
                if (self.currentTag() != .eof) self.advance();
            },
        }

        const rhs = try self.parseExpr();
        _ = try self.expect(.semicolon);

        return self.addNode(.assign, op_tok, .{ .lhs = target, .rhs = rhs });
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
            lhs = try self.addNode(.binary, op_tok, .{ .lhs = lhs, .rhs = rhs });
        }
        return lhs;
    }

    fn parsePrimary(self: *Parser) std.mem.Allocator.Error!Ast.Node.Index {
        switch (self.currentTag()) {
            .identifier => {
                const tok = self.currentTokenIndex();
                self.advance();
                const node = try self.addNode(.identifier, tok, .{ .lhs = 0, .rhs = 0 });
                return self.parsePostfix(node);
            },
            .number_literal => {
                const tok = self.currentTokenIndex();
                self.advance();
                return self.addNode(.number_literal, tok, .{ .lhs = 0, .rhs = 0 });
            },
            .builtin => {
                const tok = self.currentTokenIndex();
                self.advance();
                const node = try self.addNode(.builtin_ref, tok, .{ .lhs = 0, .rhs = 0 });
                return self.parsePostfix(node);
            },
            .period => {
                const period_tok = self.currentTokenIndex();
                self.advance();
                const name_tok = try self.expect(.identifier);
                const node = try self.addNode(.enum_literal, name_tok, .{ .lhs = period_tok, .rhs = 0 });
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
                return self.addNode(.@"comptime", comptime_tok, .{ .lhs = inner, .rhs = 0 });
            },
            else => {
                try self.addError(self.token_index, "expected expression");
                if (self.currentTag() != .eof) self.advance();
                return self.addNode(.@"error", self.currentTokenIndex(), .{ .lhs = 0, .rhs = 0 });
            },
        }
    }

    fn parsePostfix(self: *Parser, base: Ast.Node.Index) std.mem.Allocator.Error!Ast.Node.Index {
        var expr = base;
        while (true) {
            switch (self.currentTag()) {
                .l_paren => {
                    const l_paren = try self.expect(.l_paren);
                    var args = std.ArrayList(Ast.Node.Index).empty;
                    defer args.deinit(self.allocator);

                    if (self.currentTag() != .r_paren) {
                        while (true) {
                            const arg = try self.parseExpr();
                            try args.append(self.allocator, arg);
                            if (self.match(.comma)) {
                                if (self.currentTag() == .r_paren) break;
                                continue;
                            }
                            break;
                        }
                    }

                    _ = try self.expect(.r_paren);
                    const args_list = try self.addList(args.items);
                    expr = try self.addNode(.call, l_paren, .{ .lhs = expr, .rhs = args_list });
                },
                .period => {
                    const period_tok = try self.expect(.period);
                    const name_tok = try self.expect(.identifier);
                    expr = try self.addNode(.field_access, period_tok, .{ .lhs = expr, .rhs = name_tok });
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
