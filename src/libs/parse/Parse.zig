const std = @import("std");
const Ast = @import("Ast.zig");
const tokenizer = @import("tokenizer.zig");

const Token = tokenizer.Token;

pub const Error = error{ParseError} || std.mem.Allocator.Error;

// Represents in-progress parsing, converted to an Ast after completion.
gpa: std.mem.Allocator,
source: [:0]const u8,
tokens: Ast.TokenList.Slice,
tok_i: usize,
errors: std.ArrayList(Ast.Error),
nodes: Ast.NodeList,
extra_data: std.ArrayList(u32),

pub fn init(gpa: std.mem.Allocator, source: [:0]const u8, tokens: Ast.TokenList.Slice) @This() {
    var p: @This() = .{
        .gpa = gpa,
        .source = source,
        .tokens = tokens,
        .tok_i = 0,
        .errors = std.ArrayList(Ast.Error).empty,
        .nodes = Ast.NodeList.empty,
        .extra_data = std.ArrayList(u32).empty,
    };
    p.skipTrivia();
    return p;
}

pub fn parseRoot(p: *@This()) !void {
    try p.nodes.append(p.gpa, .{
        .tag = .root,
        .main_token = 0,
        .data = .{ .root = .{ .decls = 0 } },
    });
    const root_list = try p.parseRootList();
    p.nodes.set(0, .{
        .tag = .root,
        .main_token = 0,
        .data = .{ .root = .{ .decls = root_list } },
    });
}

fn currentTag(p: *const @This()) Token.Tag {
    return p.tokens.items(.tag)[p.tok_i];
}

fn currentTokenIndex(p: *const @This()) Ast.TokenIndex {
    return @intCast(p.tok_i);
}

fn advance(p: *@This()) void {
    if (p.tok_i + 1 < p.tokens.len) {
        p.tok_i += 1;
    }
    p.skipTrivia();
}

fn skipTrivia(p: *@This()) void {
    while (p.tok_i < p.tokens.len) {
        switch (p.currentTag()) {
            .doc_comment, .container_doc_comment => p.tok_i += 1,
            else => return,
        }
    }
}

fn addError(p: *@This(), token_index: usize, tag: Ast.Error.Tag, extra: Ast.Error.Extra) !void {
    try p.errors.append(p.gpa, .{
        .tag = tag,
        .token = @intCast(token_index),
        .extra = extra,
    });
}

fn addErrorExpectedToken(p: *@This(), token_index: usize, expected: Token.Tag, token_is_prev: bool) !void {
    try p.errors.append(p.gpa, .{
        .tag = .expected_token,
        .token = @intCast(token_index),
        .token_is_prev = token_is_prev,
        .extra = .{ .expected_tag = expected },
    });
}

fn addNode(p: *@This(), tag: Ast.Node.Tag, main_token: Ast.TokenIndex, data: Ast.Node.Data) !Ast.Node.Index {
    const idx = p.nodes.len;
    try p.nodes.append(p.gpa, .{
        .tag = tag,
        .main_token = main_token,
        .data = data,
    });
    return @enumFromInt(@as(u32, @intCast(idx)));
}

fn addList(p: *@This(), items: []const u32) !u32 {
    const start = p.extra_data.items.len;
    try p.extra_data.append(p.gpa, @intCast(items.len));
    for (items) |item| {
        try p.extra_data.append(p.gpa, item);
    }
    return @intCast(start);
}

fn expect(p: *@This(), tag: Token.Tag) !Ast.TokenIndex {
    const idx = p.tok_i;
    if (p.currentTag() == tag) {
        p.advance();
        return @intCast(idx);
    }
    try p.addErrorExpectedToken(idx, tag, false);
    if (p.currentTag() != .eof) p.advance();
    return @intCast(idx);
}

fn match(p: *@This(), tag: Token.Tag) bool {
    if (p.currentTag() == tag) {
        p.advance();
        return true;
    }
    return false;
}

fn syncToTopLevel(p: *@This()) void {
    while (true) {
        switch (p.currentTag()) {
            .keyword_const, .keyword_pub, .keyword_module, .eof => return,
            .semicolon => {
                p.advance();
                return;
            },
            else => p.advance(),
        }
    }
}

fn syncToStatementEnd(p: *@This()) void {
    while (true) {
        switch (p.currentTag()) {
            .semicolon => {
                p.advance();
                return;
            },
            .r_brace, .eof => return,
            else => p.advance(),
        }
    }
}

fn parseRootList(p: *@This()) !u32 {
    var items = std.ArrayList(u32).empty;
    defer items.deinit(p.gpa);

    while (p.currentTag() != .eof) {
        const decl = try p.parseTopLevelDecl();
        try items.append(p.gpa, @intFromEnum(decl));
    }

    return p.addList(items.items);
}

fn parseTopLevelDecl(p: *@This()) !Ast.Node.Index {
    if (p.currentTag() == .keyword_comptime) {
        const comptime_tok = try p.expect(.keyword_comptime);
        const inner = try p.parseTopLevelDecl();
        return p.addNode(.@"comptime", comptime_tok, .{ .unary = inner });
    }

    return switch (p.currentTag()) {
        .keyword_const => p.parseConstDecl(),
        .keyword_pub => blk: {
            _ = try p.expect(.keyword_pub);
            if (p.currentTag() != .keyword_module) {
                try p.addError(p.tok_i, .expected_module_after_pub, .{ .none = {} });
                if (p.currentTag() != .eof) p.advance();
                break :blk p.addNode(.@"error", p.currentTokenIndex(), .{ .none = {} });
            }
            break :blk p.parseModuleDecl(true);
        },
        .keyword_module => p.parseModuleDecl(false),
        else => blk: {
            try p.addError(p.tok_i, .expected_top_level, .{ .none = {} });
            if (p.currentTag() != .eof) p.advance();
            break :blk p.addNode(.@"error", p.currentTokenIndex(), .{ .none = {} });
        },
    };
}

fn parseConstDecl(p: *@This()) !Ast.Node.Index {
    const const_tok = try p.expect(.keyword_const);
    const name_tok = try p.expect(.identifier);
    _ = try p.expect(.equal);
    const value_expr = try p.parseExpr();
    if (!p.match(.semicolon)) {
        try p.addErrorExpectedToken(p.tok_i, .semicolon, false);
        p.syncToTopLevel();
    }
    return p.addNode(.const_decl, const_tok, .{ .const_decl = .{ .name = name_tok, .value = value_expr } });
}

fn parseVarDecl(p: *@This()) !Ast.Node.Index {
    const var_tok = try p.expect(.keyword_var);
    const name_tok = try p.expect(.identifier);
    _ = try p.expect(.colon);
    const ty_expr = try p.parseExpr();
    if (!p.match(.semicolon)) {
        try p.addErrorExpectedToken(p.tok_i, .semicolon, false);
        p.syncToStatementEnd();
    }
    return p.addNode(.var_decl, var_tok, .{ .var_decl = .{ .name = name_tok, .ty = ty_expr } });
}

fn parseModuleDecl(p: *@This(), is_pub: bool) !Ast.Node.Index {
    _ = try p.expect(.keyword_module);
    const name_tok = try p.expect(.identifier);
    _ = try p.expect(.l_paren);

    var ports = std.ArrayList(u32).empty;
    defer ports.deinit(p.gpa);

    if (p.currentTag() != .r_paren) {
        while (true) {
            const port = try p.parsePort();
            try ports.append(p.gpa, @intFromEnum(port));
            if (p.match(.comma)) {
                if (p.currentTag() == .r_paren) break;
                continue;
            }
            break;
        }
    }

    _ = try p.expect(.r_paren);

    const ports_list = try p.addList(ports.items);
    const body_block = try p.parseBlock();

    return p.addNode(if (is_pub) .pub_module_decl else .module_decl, name_tok, .{ .module_decl = .{ .ports = ports_list, .body = body_block } });
}

fn parsePort(p: *@This()) !Ast.Node.Index {
    const name_tok = try p.expect(.identifier);
    _ = try p.expect(.colon);

    const dir_tag = p.currentTag();
    switch (dir_tag) {
        .keyword_input, .keyword_output, .keyword_inout => p.advance(),
        else => {
            try p.addError(p.tok_i, .expected_direction, .{ .none = {} });
            if (p.currentTag() != .eof) p.advance();
        },
    }

    const ty_expr = try p.parseExpr();
    return p.addNode(.port, name_tok, .{ .port = .{ .dir = @as(u32, @intFromEnum(dir_tag)), .ty = ty_expr } });
}

fn parseBlock(p: *@This()) std.mem.Allocator.Error!Ast.Node.Index {
    const l_brace = try p.expect(.l_brace);

    var statements = std.ArrayList(u32).empty;
    defer statements.deinit(p.gpa);

    while (p.currentTag() != .r_brace and p.currentTag() != .eof) {
        const stmt = try p.parseStatement();
        try statements.append(p.gpa, @intFromEnum(stmt));
    }

    _ = try p.expect(.r_brace);
    const list_index = try p.addList(statements.items);
    return p.addNode(.block, l_brace, .{ .block = .{ .statements = list_index } });
}

fn parseStatement(p: *@This()) std.mem.Allocator.Error!Ast.Node.Index {
    if (p.currentTag() == .keyword_comptime) {
        const comptime_tok = try p.expect(.keyword_comptime);
        const inner = try p.parseStatement();
        return p.addNode(.@"comptime", comptime_tok, .{ .unary = inner });
    }

    return switch (p.currentTag()) {
        .keyword_var => p.parseVarDecl(),
        .keyword_const => p.parseConstDecl(),
        .keyword_always_ff => p.parseAlwaysFF(),
        .keyword_comb => p.parseComb(),
        .keyword_if => p.parseIf(),
        .keyword_if_reset => p.parseIfReset(),
        .l_brace => p.parseBlock(),
        .identifier => p.parseAssign(),
        else => blk: {
            try p.addError(p.tok_i, .expected_statement, .{ .none = {} });
            if (p.currentTag() != .eof) p.advance();
            break :blk p.addNode(.@"error", p.currentTokenIndex(), .{ .none = {} });
        },
    };
}

fn parseAlwaysFF(p: *@This()) !Ast.Node.Index {
    const tok = try p.expect(.keyword_always_ff);
    const block = try p.parseBlock();
    return p.addNode(.always_ff, tok, .{ .unary = block });
}

fn parseComb(p: *@This()) !Ast.Node.Index {
    const tok = try p.expect(.keyword_comb);
    const block = try p.parseBlock();
    return p.addNode(.comb, tok, .{ .unary = block });
}

fn parseIf(p: *@This()) !Ast.Node.Index {
    const tok = try p.expect(.keyword_if);
    const cond = try p.parseExpr();
    const then_block = try p.parseBlock();

    var else_block: Ast.Node.OptionalIndex = .none;
    if (p.match(.keyword_else)) {
        const block = try p.parseBlock();
        else_block = block.toOptional();
    }

    return p.addNode(.if_stmt, tok, .{ .if_stmt = .{ .cond = cond, .then_block = then_block, .else_block = else_block } });
}

fn parseIfReset(p: *@This()) !Ast.Node.Index {
    const tok = try p.expect(.keyword_if_reset);
    const then_block = try p.parseBlock();

    var else_block: Ast.Node.OptionalIndex = .none;
    if (p.match(.keyword_else)) {
        const block = try p.parseBlock();
        else_block = block.toOptional();
    }

    return p.addNode(.if_reset, tok, .{ .if_reset = .{ .then_block = then_block, .else_block = else_block } });
}

fn parseAssign(p: *@This()) !Ast.Node.Index {
    const name_tok = try p.expect(.identifier);
    const target = try p.addNode(.identifier, name_tok, .{ .none = {} });

    const op_tok = p.currentTokenIndex();
    const op_tag = p.currentTag();
    switch (op_tag) {
        .equal, .plus_equal => p.advance(),
        else => {
            try p.addError(p.tok_i, .expected_assignment_op, .{ .none = {} });
            if (p.currentTag() != .eof) p.advance();
        },
    }

    const rhs = try p.parseExpr();
    if (!p.match(.semicolon)) {
        try p.addErrorExpectedToken(p.tok_i, .semicolon, false);
        p.syncToStatementEnd();
    }

    return p.addNode(.assign, op_tok, .{ .assign = .{ .target = target, .value = rhs } });
}

fn parseExpr(p: *@This()) std.mem.Allocator.Error!Ast.Node.Index {
    return p.parseBinaryExpr(1);
}

fn parseBinaryExpr(p: *@This(), min_prec: u8) std.mem.Allocator.Error!Ast.Node.Index {
    var lhs = try p.parsePrimary();
    while (true) {
        const op_tag = p.currentTag();
        const prec = precedence(op_tag);
        if (prec < min_prec) break;

        const op_tok = p.currentTokenIndex();
        p.advance();
        const rhs = try p.parseBinaryExpr(prec + 1);
        lhs = try p.addNode(.binary, op_tok, .{ .binary = .{ .lhs = lhs, .rhs = rhs } });
    }
    return lhs;
}

fn parsePrimary(p: *@This()) std.mem.Allocator.Error!Ast.Node.Index {
    switch (p.currentTag()) {
        .identifier => {
            const tok = p.currentTokenIndex();
            p.advance();
            const node = try p.addNode(.identifier, tok, .{ .none = {} });
            return p.parsePostfix(node);
        },
        .number_literal => {
            const tok = p.currentTokenIndex();
            p.advance();
            return p.addNode(.number_literal, tok, .{ .none = {} });
        },
        .builtin => {
            const tok = p.currentTokenIndex();
            p.advance();
            const node = try p.addNode(.builtin_ref, tok, .{ .none = {} });
            return p.parsePostfix(node);
        },
        .period => {
            p.advance();
            const name_tok = try p.expect(.identifier);
            const node = try p.addNode(.enum_literal, name_tok, .{ .none = {} });
            return p.parsePostfix(node);
        },
        .l_paren => {
            _ = try p.expect(.l_paren);
            const expr = try p.parseExpr();
            _ = try p.expect(.r_paren);
            return expr;
        },
        .keyword_comptime => {
            const comptime_tok = try p.expect(.keyword_comptime);
            const inner = try p.parsePrimary();
            return p.addNode(.@"comptime", comptime_tok, .{ .unary = inner });
        },
        else => {
            try p.addError(p.tok_i, .expected_expression, .{ .none = {} });
            if (p.currentTag() != .eof) p.advance();
            return p.addNode(.@"error", p.currentTokenIndex(), .{ .none = {} });
        },
    }
}

fn parsePostfix(p: *@This(), base: Ast.Node.Index) std.mem.Allocator.Error!Ast.Node.Index {
    var expr = base;
    while (true) {
        switch (p.currentTag()) {
            .l_paren => {
                const l_paren = try p.expect(.l_paren);
                var args = std.ArrayList(u32).empty;
                defer args.deinit(p.gpa);

                if (p.currentTag() != .r_paren) {
                    while (true) {
                        const arg = try p.parseExpr();
                        try args.append(p.gpa, @intFromEnum(arg));
                        if (p.match(.comma)) {
                            if (p.currentTag() == .r_paren) break;
                            continue;
                        }
                        break;
                    }
                }

                _ = try p.expect(.r_paren);
                const args_list = try p.addList(args.items);
                expr = try p.addNode(.call, l_paren, .{ .call = .{ .callee = expr, .args = args_list } });
            },
            .period => {
                const period_tok = try p.expect(.period);
                const name_tok = try p.expect(.identifier);
                expr = try p.addNode(.field_access, period_tok, .{ .field_access = .{ .lhs = expr, .field = name_tok } });
            },
            else => return expr,
        }
    }
}

fn precedence(tag: Token.Tag) u8 {
    return switch (tag) {
        .equal_equal => 1,
        .plus, .minus => 2,
        else => 0,
    };
}
