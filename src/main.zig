const std = @import("std");
const zerilog = @import("zerilog");

const tokenizer_mod = zerilog.tokenizer;
const Tokenizer = tokenizer_mod.Tokenizer;
const Token = tokenizer_mod.Token;
const TokenTag = Token.Tag;

const Allocator = std.mem.Allocator;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var stdout_buffer: [1024]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        std.debug.print("usage: zerilog <source.zer>\n", .{});
        return;
    }

    const path = args[1];

    var file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    var source = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(source);

    const source_len = source.len;
    source = try allocator.realloc(source, source_len + 1);
    source[source_len] = 0;
    const source_bytes = source[0..source_len];
    const source_sentinel = source[0..source_len :0];

    var tokenizer_state = Tokenizer.init(source_sentinel);
    var parser = Parser.init(allocator, &tokenizer_state, source_bytes);

    const module = try parser.parseModule();
    defer {
        allocator.free(module.ports);
        allocator.free(module.vars);
        for (module.always_blocks) |ab| {
            allocator.free(ab.reset_assigns);
            allocator.free(ab.normal_assigns);
        }
        allocator.free(module.always_blocks);
    }

    try codegenSv(stdout, module);
    try stdout.flush();
}

// ======= AST & 型 =======

const Direction = enum { input, output, inout };

const IntSignedness = enum { unsigned, signed };

const TypeKind = union(enum) {
    clock,
    reset,
    bit,
    int: struct {
        signedness: IntSignedness,
        bits: u16,
    },
    logic: struct {
        signedness: IntSignedness,
        bits: u16,
    },
};

const Port = struct {
    name: []const u8,
    dir: Direction,
    ty: TypeKind,
};

const VarDecl = struct {
    name: []const u8,
    ty: TypeKind,
};

const Expr = union(enum) {
    ident: []const u8,
    number: u32,
};

const Assignment = struct {
    name: []const u8,
    rhs: Expr,
};

const AlwaysBlock = struct {
    reset_assigns: []Assignment,
    normal_assigns: []Assignment,
};

const Module = struct {
    name: []const u8,
    ports: []Port,
    vars: []VarDecl,
    always_blocks: []AlwaysBlock,
};

// ======= エラー =======

const ParserError = Allocator.Error || std.fmt.ParseIntError || error{
    UnexpectedToken,
    InvalidSignedness,
    InvalidBitWidth,
    InvalidLogicInnerType,
    UnknownType,
    UnknownBuiltinType,
    ExpectedType,
    ExpectedDirection,
    UnexpectedEof,
    ExpectedExpr,
};

// ======= logicN / slogicN 糖衣構文 =======

fn parseLogicSugar(name: []const u8) ?TypeKind {
    // logic / logic1
    if (std.mem.eql(u8, name, "logic") or std.mem.eql(u8, name, "logic1")) {
        return TypeKind{
            .logic = .{ .signedness = .unsigned, .bits = 1 },
        };
    }

    // logic8, logic32, ...
    if (std.mem.startsWith(u8, name, "logic")) {
        const digits = name["logic".len..];
        if (digits.len == 0) return null;

        for (digits) |ch| {
            if (!std.ascii.isDigit(ch)) return null;
        }

        const bits = std.fmt.parseUnsigned(u16, digits, 10) catch return null;
        if (bits == 0) return null;

        return TypeKind{
            .logic = .{
                .signedness = .unsigned,
                .bits = bits,
            },
        };
    }

    // slogic / slogic1
    if (std.mem.eql(u8, name, "slogic") or std.mem.eql(u8, name, "slogic1")) {
        return TypeKind{
            .logic = .{ .signedness = .signed, .bits = 1 },
        };
    }

    // slogic8, slogic32, ...
    if (std.mem.startsWith(u8, name, "slogic")) {
        const digits = name["slogic".len..];
        if (digits.len == 0) return null;

        for (digits) |ch| {
            if (!std.ascii.isDigit(ch)) return null;
        }

        const bits = std.fmt.parseUnsigned(u16, digits, 10) catch return null;
        if (bits == 0) return null;

        return TypeKind{
            .logic = .{
                .signedness = .signed,
                .bits = bits,
            },
        };
    }

    return null;
}

// ======= Parser =======

const Parser = struct {
    allocator: Allocator,
    tokenizer: *Tokenizer,
    source: []const u8,
    current: Token,

    pub fn init(allocator: Allocator, tokenizer: *Tokenizer, source: []const u8) Parser {
        var p = Parser{
            .allocator = allocator,
            .tokenizer = tokenizer,
            .source = source,
            .current = undefined,
        };
        p.advance();
        return p;
    }

    fn lexeme(self: *Parser, tok: Token) []const u8 {
        return self.source[tok.loc.start..tok.loc.end];
    }

    fn currentIsIdentifier(self: *Parser, name: []const u8) bool {
        return self.current.tag == .identifier and std.mem.eql(u8, self.lexeme(self.current), name);
    }

    fn expectIdentifierKeyword(self: *Parser, name: []const u8) !void {
        if (!self.currentIsIdentifier(name)) {
            return error.UnexpectedToken;
        }
        self.advance();
    }

    fn advance(self: *Parser) void {
        self.current = self.tokenizer.next();
    }

    fn expect(self: *Parser, tag: TokenTag) !Token {
        if (self.current.tag != tag) {
            return error.UnexpectedToken;
        }
        const tok = self.current;
        self.advance();
        return tok;
    }

    fn check(self: *Parser, tag: TokenTag) bool {
        return self.current.tag == tag;
    }

    fn match(self: *Parser, tag: TokenTag) bool {
        if (self.check(tag)) {
            self.advance();
            return true;
        }
        return false;
    }

    fn parseIntType(self: *Parser) ParserError!TypeKind {
        _ = try self.expect(.l_paren);

        _ = try self.expect(.period);
        const sign_tok = try self.expect(.identifier);
        const sign = self.lexeme(sign_tok);

        var signedness: IntSignedness = undefined;
        if (std.mem.eql(u8, sign, "unsigned")) {
            signedness = .unsigned;
        } else if (std.mem.eql(u8, sign, "signed")) {
            signedness = .signed;
        } else {
            return error.InvalidSignedness;
        }

        _ = try self.expect(.comma);
        const bits_tok = try self.expect(.number_literal);
        const bits = try std.fmt.parseUnsigned(u16, self.lexeme(bits_tok), 10);
        if (bits == 0) return error.InvalidBitWidth;

        _ = try self.expect(.r_paren);

        return TypeKind{
            .int = .{
                .signedness = signedness,
                .bits = bits,
            },
        };
    }

    fn parseLogicType(self: *Parser) ParserError!TypeKind {
        _ = try self.expect(.l_paren);

        _ = try self.expect(.period);
        const sign_tok = try self.expect(.identifier);
        const sign = self.lexeme(sign_tok);

        var signedness: IntSignedness = undefined;
        if (std.mem.eql(u8, sign, "unsigned")) {
            signedness = .unsigned;
        } else if (std.mem.eql(u8, sign, "signed")) {
            signedness = .signed;
        } else {
            return error.InvalidSignedness;
        }

        _ = try self.expect(.comma);
        const bits_tok = try self.expect(.number_literal);
        const bits = try std.fmt.parseUnsigned(u16, self.lexeme(bits_tok), 10);
        if (bits == 0) return error.InvalidBitWidth;

        _ = try self.expect(.r_paren);

        return TypeKind{
            .logic = .{
                .signedness = signedness,
                .bits = bits,
            },
        };
    }

    fn parseType(self: *Parser) ParserError!TypeKind {
        switch (self.current.tag) {
            .identifier => {
                const name_tok = self.current;
                self.advance();
                const name = self.lexeme(name_tok);

                if (std.mem.eql(u8, name, "Clock")) {
                    return .clock;
                } else if (std.mem.eql(u8, name, "Reset")) {
                    return .reset;
                } else if (std.mem.eql(u8, name, "bit")) {
                    return .bit;
                } else if (parseLogicSugar(name)) |ty| {
                    return ty;
                } else {
                    return error.UnknownType;
                }
            },
            .builtin => {
                const builtin_tok = self.current;
                self.advance();
                const raw_name = self.lexeme(builtin_tok);
                if (raw_name.len <= 1 or raw_name[0] != '@') {
                    return error.UnknownBuiltinType;
                }
                const bname = raw_name[1..];

                if (std.mem.eql(u8, bname, "Int")) {
                    return self.parseIntType();
                } else if (std.mem.eql(u8, bname, "Logic")) {
                    return self.parseLogicType();
                } else {
                    return error.UnknownBuiltinType;
                }
            },
            else => return error.ExpectedType,
        }
    }

    fn parseExpr(self: *Parser) ParserError!Expr {
        return switch (self.current.tag) {
            .identifier => blk: {
                const tok = self.current;
                self.advance();
                break :blk Expr{ .ident = self.lexeme(tok) };
            },
            .number_literal => blk: {
                const tok = self.current;
                self.advance();
                const value = try std.fmt.parseUnsigned(u32, self.lexeme(tok), 10);
                break :blk Expr{ .number = value };
            },
            else => error.ExpectedExpr,
        };
    }

    fn parseVarDecl(self: *Parser, vars_list: *std.ArrayList(VarDecl)) ParserError!void {
        _ = try self.expect(.keyword_var);
        const name_tok = try self.expect(.identifier);
        _ = try self.expect(.colon);
        const ty = try self.parseType();

        // optional initializer: "= expr;"
        if (self.match(.equal)) {
            _ = try self.parseExpr(); // 今は無視
        }
        _ = try self.expect(.semicolon);

        try vars_list.append(self.allocator, .{
            .name = self.lexeme(name_tok),
            .ty = ty,
        });
    }

    fn parseAssignList(self: *Parser, list: *std.ArrayList(Assignment)) ParserError!void {
        while (!self.check(.r_brace) and self.current.tag != .eof) {
            if (self.current.tag == .identifier) {
                const name_tok = self.current;
                self.advance();
                _ = try self.expect(.equal);
                const rhs = try self.parseExpr();
                _ = try self.expect(.semicolon);

                try list.append(self.allocator, .{
                    .name = self.lexeme(name_tok),
                    .rhs = rhs,
                });
            } else {
                // よく分からないものはとりあえず読み飛ばす
                self.advance();
            }
        }
    }

    fn parseAlwaysBlock(self: *Parser, always_list: *std.ArrayList(AlwaysBlock)) ParserError!void {
        try self.expectIdentifierKeyword("always_ff");
        _ = try self.expect(.l_brace);

        // if_reset ブロック
        try self.expectIdentifierKeyword("if_reset");
        _ = try self.expect(.l_brace);

        var reset_list = std.ArrayList(Assignment).empty;
        defer reset_list.deinit(self.allocator);
        try self.parseAssignList(&reset_list);
        _ = try self.expect(.r_brace);

        // else ブロック（任意）
        var normal_list = std.ArrayList(Assignment).empty;
        defer normal_list.deinit(self.allocator);
        if (self.match(.keyword_else)) {
            _ = try self.expect(.l_brace);
            try self.parseAssignList(&normal_list);
            _ = try self.expect(.r_brace);
        }

        _ = try self.expect(.r_brace); // always_ff の閉じ

        const reset_slice = try reset_list.toOwnedSlice(self.allocator);
        const normal_slice = try normal_list.toOwnedSlice(self.allocator);

        try always_list.append(self.allocator, .{
            .reset_assigns = reset_slice,
            .normal_assigns = normal_slice,
        });
    }

    pub fn parseModule(self: *Parser) ParserError!Module {
        // 最初の pub/module までスキップ（const Reset = ... などを飛ばす）
        while (self.current.tag != .keyword_module and
            self.current.tag != .keyword_pub and
            self.current.tag != .eof)
        {
            self.advance();
        }
        if (self.current.tag == .eof) return error.UnexpectedEof;

        // optional 'pub'
        _ = self.match(.keyword_pub);

        _ = try self.expect(.keyword_module);
        const name_tok = try self.expect(.identifier);
        const name = self.lexeme(name_tok);

        _ = try self.expect(.l_paren);

        var ports_list = std.ArrayList(Port).empty;
        defer ports_list.deinit(self.allocator);

        if (!self.check(.r_paren)) {
            while (true) {
                const port_name_tok = try self.expect(.identifier);
                _ = try self.expect(.colon);

                const dir_tok = self.current;
                var dir: Direction = undefined;
                switch (dir_tok.tag) {
                    .keyword_input => dir = .input,
                    .keyword_output => dir = .output,
                    .keyword_inout => dir = .inout,
                    else => return error.ExpectedDirection,
                }
                self.advance();

                const ty = try self.parseType();

                try ports_list.append(self.allocator, .{
                    .name = self.lexeme(port_name_tok),
                    .dir = dir,
                    .ty = ty,
                });

                if (self.match(.comma)) {
                    if (self.check(.r_paren)) {
                        // trailing comma OK
                        break;
                    }
                    continue;
                } else {
                    break;
                }
            }
        }

        _ = try self.expect(.r_paren);

        const ports_slice = try ports_list.toOwnedSlice(self.allocator);

        // モジュール本体
        _ = try self.expect(.l_brace);

        var vars_list = std.ArrayList(VarDecl).empty;
        defer vars_list.deinit(self.allocator);

        var always_list = std.ArrayList(AlwaysBlock).empty;
        defer always_list.deinit(self.allocator);

        while (!self.check(.r_brace) and self.current.tag != .eof) {
            if (self.current.tag == .keyword_var) {
                try self.parseVarDecl(&vars_list);
            } else if (self.currentIsIdentifier("always_ff")) {
                try self.parseAlwaysBlock(&always_list);
            } else {
                self.advance(); // TODO: comb などは後で
            }
        }

        _ = try self.expect(.r_brace);

        const vars_slice = try vars_list.toOwnedSlice(self.allocator);
        const always_slice = try always_list.toOwnedSlice(self.allocator);

        return Module{
            .name = name,
            .ports = ports_slice,
            .vars = vars_slice,
            .always_blocks = always_slice,
        };
    }
};

// ======= SystemVerilog 出力 =======

fn writeTypeAndName(writer: *std.Io.Writer, ty: TypeKind, name: []const u8) !void {
    switch (ty) {
        .clock, .reset => {
            try writer.writeAll("logic ");
            try writer.print("{s}", .{name});
        },
        .bit => {
            try writer.writeAll("bit ");
            try writer.print("{s}", .{name});
        },
        .int => |info| {
            try writer.writeAll("bit");
            if (info.signedness == .signed) {
                try writer.writeAll(" signed");
            }
            if (info.bits > 1) {
                const msb: u16 = info.bits - 1;
                try writer.print(" [{d}:0]", .{msb});
            }
            try writer.print(" {s}", .{name});
        },
        .logic => |info| {
            try writer.writeAll("logic");
            if (info.signedness == .signed) {
                try writer.writeAll(" signed");
            }
            if (info.bits > 1) {
                const msb: u16 = info.bits - 1;
                try writer.print(" [{d}:0]", .{msb});
            }
            try writer.print(" {s}", .{name});
        },
    }
}

fn codegenExpr(writer: *std.Io.Writer, expr: Expr) !void {
    switch (expr) {
        .ident => |name| try writer.print("{s}", .{name}),
        .number => |val| try writer.print("{d}", .{val}),
    }
}

fn codegenSv(writer: *std.Io.Writer, module: Module) !void {
    // ポートリスト
    try writer.print("module {s} (\n", .{module.name});

    for (module.ports, 0..) |port, idx| {
        const is_last = idx + 1 == module.ports.len;

        try writer.writeAll("    ");

        switch (port.dir) {
            .input => try writer.writeAll("input "),
            .output => try writer.writeAll("output "),
            .inout => try writer.writeAll("inout "),
        }

        try writeTypeAndName(writer, port.ty, port.name);

        if (!is_last) {
            try writer.writeAll(",\n");
        } else {
            try writer.writeAll("\n");
        }
    }

    try writer.writeAll(");\n");

    // ローカル変数宣言
    if (module.vars.len > 0) {
        try writer.writeAll("\n");
        for (module.vars) |v| {
            try writer.writeAll("    ");
            try writeTypeAndName(writer, v.ty, v.name);
            try writer.writeAll(";\n");
        }
    }

    // クロック / リセット名探索
    var clk_name: ?[]const u8 = null;
    var rst_name: ?[]const u8 = null;
    for (module.ports) |p| {
        switch (p.ty) {
            .clock => clk_name = p.name,
            .reset => rst_name = p.name,
            else => {},
        }
    }

    if (module.always_blocks.len > 0) {
        if (clk_name == null) return error.MissingClock;
        if (rst_name == null) return error.MissingReset;
    }

    // always_ff ブロック生成
    for (module.always_blocks) |ab| {
        try writer.print(
            "\n    always_ff @(posedge {s} or negedge {s}) begin\n",
            .{ clk_name.?, rst_name.? },
        );

        // ResetActiveLow 固定: if (!rst)
        try writer.print("        if (!{s}) begin\n", .{rst_name.?});
        for (ab.reset_assigns) |asgn| {
            try writer.writeAll("            ");
            try writer.print("{s} <= ", .{asgn.name});
            try codegenExpr(writer, asgn.rhs);
            try writer.writeAll(";\n");
        }
        try writer.writeAll("        end else begin\n");
        for (ab.normal_assigns) |asgn| {
            try writer.writeAll("            ");
            try writer.print("{s} <= ", .{asgn.name});
            try codegenExpr(writer, asgn.rhs);
            try writer.writeAll(";\n");
        }
        try writer.writeAll("        end\n");
        try writer.writeAll("    end\n");
    }

    try writer.writeAll("endmodule\n");
}
