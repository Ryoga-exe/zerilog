const std = @import("std");
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
        std.debug.print("usage: zerilogc <source.zog>\n", .{});
        return;
    }

    const path = args[1];

    var file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    const source = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(source);

    var tokenizer = Tokenizer.init(source);
    var parser = Parser.init(allocator, &tokenizer);

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

// ======= Lexer =======

const TokenTag = enum {
    eof,
    identifier,
    number,

    kw_pub,
    kw_module,
    kw_input,
    kw_output,
    kw_inout,
    kw_var,
    kw_always_ff,
    kw_if_reset,
    kw_else,

    l_paren,
    r_paren,
    l_brace,
    r_brace,
    l_bracket,
    r_bracket,
    colon,
    comma,
    at,
    dot,
    equal,
    semicolon,
};

const Token = struct {
    tag: TokenTag,
    lexeme: []const u8,
    line: usize,
    column: usize,
};

const Tokenizer = struct {
    input: []const u8,
    index: usize,
    line: usize,
    column: usize,

    pub fn init(input: []const u8) Tokenizer {
        return .{
            .input = input,
            .index = 0,
            .line = 1,
            .column = 1,
        };
    }

    fn peek(self: *Tokenizer) ?u8 {
        if (self.index >= self.input.len) return null;
        return self.input[self.index];
    }

    fn advance(self: *Tokenizer) ?u8 {
        const c = self.peek() orelse return null;
        self.index += 1;
        if (c == '\n') {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
        return c;
    }

    fn skipWhitespace(self: *Tokenizer) void {
        while (true) {
            const c_opt = self.peek();
            if (c_opt == null) break;
            const c = c_opt.?;
            switch (c) {
                ' ', '\t', '\r', '\n' => {
                    _ = self.advance();
                },
                else => return,
            }
        }
    }

    fn isIdentStart(c: u8) bool {
        return std.ascii.isAlphabetic(c) or c == '_';
    }

    fn isIdentChar(c: u8) bool {
        return std.ascii.isAlphanumeric(c) or c == '_';
    }

    pub fn next(self: *Tokenizer) Token {
        self.skipWhitespace();

        const start_line = self.line;
        const start_column = self.column;

        const c_opt = self.peek();
        if (c_opt == null) {
            return .{
                .tag = .eof,
                .lexeme = self.input[self.index..self.index],
                .line = start_line,
                .column = start_column,
            };
        }
        const c = c_opt.?;

        // punctuation
        switch (c) {
            '(' => {
                _ = self.advance();
                return .{ .tag = .l_paren, .lexeme = self.input[self.index - 1 .. self.index], .line = start_line, .column = start_column };
            },
            ')' => {
                _ = self.advance();
                return .{ .tag = .r_paren, .lexeme = self.input[self.index - 1 .. self.index], .line = start_line, .column = start_column };
            },
            '{' => {
                _ = self.advance();
                return .{ .tag = .l_brace, .lexeme = self.input[self.index - 1 .. self.index], .line = start_line, .column = start_column };
            },
            '}' => {
                _ = self.advance();
                return .{ .tag = .r_brace, .lexeme = self.input[self.index - 1 .. self.index], .line = start_line, .column = start_column };
            },
            '[' => {
                _ = self.advance();
                return .{ .tag = .l_bracket, .lexeme = self.input[self.index - 1 .. self.index], .line = start_line, .column = start_column };
            },
            ']' => {
                _ = self.advance();
                return .{ .tag = .r_bracket, .lexeme = self.input[self.index - 1 .. self.index], .line = start_line, .column = start_column };
            },
            ':' => {
                _ = self.advance();
                return .{ .tag = .colon, .lexeme = self.input[self.index - 1 .. self.index], .line = start_line, .column = start_column };
            },
            ',' => {
                _ = self.advance();
                return .{ .tag = .comma, .lexeme = self.input[self.index - 1 .. self.index], .line = start_line, .column = start_column };
            },
            '@' => {
                _ = self.advance();
                return .{ .tag = .at, .lexeme = self.input[self.index - 1 .. self.index], .line = start_line, .column = start_column };
            },
            '.' => {
                _ = self.advance();
                return .{ .tag = .dot, .lexeme = self.input[self.index - 1 .. self.index], .line = start_line, .column = start_column };
            },
            '=' => {
                _ = self.advance();
                return .{ .tag = .equal, .lexeme = self.input[self.index - 1 .. self.index], .line = start_line, .column = start_column };
            },
            ';' => {
                _ = self.advance();
                return .{ .tag = .semicolon, .lexeme = self.input[self.index - 1 .. self.index], .line = start_line, .column = start_column };
            },
            else => {},
        }

        // identifier or keyword
        if (isIdentStart(c)) {
            const start_index = self.index;
            _ = self.advance();
            while (self.peek()) |pc| {
                if (!isIdentChar(pc)) break;
                _ = self.advance();
            }
            const slice = self.input[start_index..self.index];

            var tag: TokenTag = .identifier;
            if (std.mem.eql(u8, slice, "pub")) {
                tag = .kw_pub;
            } else if (std.mem.eql(u8, slice, "module")) {
                tag = .kw_module;
            } else if (std.mem.eql(u8, slice, "input")) {
                tag = .kw_input;
            } else if (std.mem.eql(u8, slice, "output")) {
                tag = .kw_output;
            } else if (std.mem.eql(u8, slice, "inout")) {
                tag = .kw_inout;
            } else if (std.mem.eql(u8, slice, "var")) {
                tag = .kw_var;
            } else if (std.mem.eql(u8, slice, "always_ff")) {
                tag = .kw_always_ff;
            } else if (std.mem.eql(u8, slice, "if_reset")) {
                tag = .kw_if_reset;
            } else if (std.mem.eql(u8, slice, "else")) {
                tag = .kw_else;
            }

            return .{
                .tag = tag,
                .lexeme = slice,
                .line = start_line,
                .column = start_column,
            };
        }

        // number
        if (std.ascii.isDigit(c)) {
            const start_index = self.index;
            _ = self.advance();
            while (self.peek()) |pc| {
                if (!std.ascii.isDigit(pc)) break;
                _ = self.advance();
            }
            const slice = self.input[start_index..self.index];
            return .{
                .tag = .number,
                .lexeme = slice,
                .line = start_line,
                .column = start_column,
            };
        }

        // 未知の文字は読み飛ばす
        _ = self.advance();
        return self.next();
    }
};

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
    current: Token,

    pub fn init(allocator: Allocator, tokenizer: *Tokenizer) Parser {
        var p = Parser{
            .allocator = allocator,
            .tokenizer = tokenizer,
            .current = undefined,
        };
        p.advance();
        return p;
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

        _ = try self.expect(.dot);
        const sign_tok = try self.expect(.identifier);

        var signedness: IntSignedness = undefined;
        if (std.mem.eql(u8, sign_tok.lexeme, "unsigned")) {
            signedness = .unsigned;
        } else if (std.mem.eql(u8, sign_tok.lexeme, "signed")) {
            signedness = .signed;
        } else {
            return error.InvalidSignedness;
        }

        _ = try self.expect(.comma);
        const bits_tok = try self.expect(.number);
        const bits = try std.fmt.parseUnsigned(u16, bits_tok.lexeme, 10);
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

        _ = try self.expect(.dot);
        const sign_tok = try self.expect(.identifier);

        var signedness: IntSignedness = undefined;
        if (std.mem.eql(u8, sign_tok.lexeme, "unsigned")) {
            signedness = .unsigned;
        } else if (std.mem.eql(u8, sign_tok.lexeme, "signed")) {
            signedness = .signed;
        } else {
            return error.InvalidSignedness;
        }

        _ = try self.expect(.comma);
        const bits_tok = try self.expect(.number);
        const bits = try std.fmt.parseUnsigned(u16, bits_tok.lexeme, 10);
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
                const name = name_tok.lexeme;

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
            .at => {
                self.advance();
                const builtin_tok = try self.expect(.identifier);
                const bname = builtin_tok.lexeme;

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
                break :blk Expr{ .ident = tok.lexeme };
            },
            .number => blk: {
                const tok = self.current;
                self.advance();
                const value = try std.fmt.parseUnsigned(u32, tok.lexeme, 10);
                break :blk Expr{ .number = value };
            },
            else => error.ExpectedExpr,
        };
    }

    fn parseVarDecl(self: *Parser, vars_list: *std.ArrayList(VarDecl)) ParserError!void {
        _ = try self.expect(.kw_var);
        const name_tok = try self.expect(.identifier);
        _ = try self.expect(.colon);
        const ty = try self.parseType();

        // optional initializer: "= expr;"
        if (self.match(.equal)) {
            _ = try self.parseExpr(); // 今は無視
        }
        _ = try self.expect(.semicolon);

        try vars_list.append(self.allocator, .{
            .name = name_tok.lexeme,
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
                    .name = name_tok.lexeme,
                    .rhs = rhs,
                });
            } else {
                // よく分からないものはとりあえず読み飛ばす
                self.advance();
            }
        }
    }

    fn parseAlwaysBlock(self: *Parser, always_list: *std.ArrayList(AlwaysBlock)) ParserError!void {
        _ = try self.expect(.kw_always_ff);
        _ = try self.expect(.l_brace);

        // if_reset ブロック
        _ = try self.expect(.kw_if_reset);
        _ = try self.expect(.l_brace);

        var reset_list = std.ArrayList(Assignment).empty;
        defer reset_list.deinit(self.allocator);
        try self.parseAssignList(&reset_list);
        _ = try self.expect(.r_brace);

        // else ブロック（任意）
        var normal_list = std.ArrayList(Assignment).empty;
        defer normal_list.deinit(self.allocator);
        if (self.match(.kw_else)) {
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
        while (self.current.tag != .kw_module and
            self.current.tag != .kw_pub and
            self.current.tag != .eof)
        {
            self.advance();
        }
        if (self.current.tag == .eof) return error.UnexpectedEof;

        // optional 'pub'
        _ = self.match(.kw_pub);

        _ = try self.expect(.kw_module);
        const name_tok = try self.expect(.identifier);
        const name = name_tok.lexeme;

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
                    .kw_input => dir = .input,
                    .kw_output => dir = .output,
                    .kw_inout => dir = .inout,
                    else => return error.ExpectedDirection,
                }
                self.advance();

                const ty = try self.parseType();

                try ports_list.append(self.allocator, .{
                    .name = port_name_tok.lexeme,
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
            switch (self.current.tag) {
                .kw_var => try self.parseVarDecl(&vars_list),
                .kw_always_ff => try self.parseAlwaysBlock(&always_list),
                else => self.advance(), // TODO: comb などは後で
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
