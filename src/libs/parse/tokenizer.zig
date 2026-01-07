const std = @import("std");

pub const Token = struct {
    tag: Tag,
    loc: Loc,

    pub const Loc = struct {
        start: usize,
        end: usize,
    };

    pub const keywords = std.StaticStringMap(Tag).initComptime(.{
        .{ "module", .keyword_module },
        .{ "pub", .keyword_pub },
        .{ "input", .keyword_input },
        .{ "output", .keyword_output },
        .{ "inout", .keyword_inout },
        .{ "var", .keyword_var },
        .{ "always_ff", .keyword_always_ff },
        .{ "if_reset", .keyword_if_reset },
        .{ "else", .keyword_else },
    });

    pub fn getKeyword(bytes: []const u8) ?Tag {
        return keywords.get(bytes);
    }

    pub const Tag = enum {
        invalid,
        eof,
        identifier,
        number_literal,
        dot,
        comma,
        colon,
        semicolon,
        at,
        equal,
        l_paren,
        r_paren,
        l_brace,
        r_brace,
        l_bracket,
        r_bracket,
        keyword_pub,
        keyword_module,
        keyword_input,
        keyword_output,
        keyword_inout,
        keyword_var,
        keyword_always_ff,
        keyword_if_reset,
        keyword_else,

        pub fn lexeme(tag: Tag) ?[]const u8 {
            return switch (tag) {
                .invalid,
                .eof,
                .identifier,
                .number_literal,
                .keyword_pub,
                .keyword_module,
                .keyword_input,
                .keyword_output,
                .keyword_inout,
                .keyword_var,
                .keyword_always_ff,
                .keyword_if_reset,
                .keyword_else,
                => null,

                .dot => ".",
                .comma => ",",
                .colon => ":",
                .semicolon => ";",
                .at => "@",
                .equal => "=",
                .l_paren => "(",
                .r_paren => ")",
                .l_brace => "{",
                .r_brace => "}",
                .l_bracket => "[",
                .r_bracket => "]",
            };
        }

        pub fn symbol(tag: Tag) []const u8 {
            return tag.lexeme() orelse switch (tag) {
                .invalid => "invalid token",
                .eof => "EOF",
                .identifier => "identifier",
                .number_literal => "number literal",
                .keyword_pub => "pub",
                .keyword_module => "module",
                .keyword_input => "input",
                .keyword_output => "output",
                .keyword_inout => "inout",
                .keyword_var => "var",
                .keyword_always_ff => "always_ff",
                .keyword_if_reset => "if_reset",
                .keyword_else => "else",
            };
        }
    };
};

pub const Tokenizer = struct {
    buffer: []const u8,
    index: usize,

    pub fn init(buffer: []const u8) Tokenizer {
        var tokenizer = Tokenizer{
            .buffer = buffer,
            .index = 0,
        };
        if (buffer.len >= 3 and std.mem.startsWith(u8, buffer, "\xEF\xBB\xBF")) {
            tokenizer.index = 3;
        }
        return tokenizer;
    }

    pub fn slice(self: *const Tokenizer, loc: Token.Loc) []const u8 {
        return self.buffer[loc.start..loc.end];
    }

    pub fn next(self: *Tokenizer) Token {
        self.skipTrivia();

        const start = self.index;
        if (start >= self.buffer.len) {
            return makeToken(.eof, start, start);
        }

        const c = self.buffer[self.index];
        switch (c) {
            '(' => return self.advanceSingle(.l_paren, start),
            ')' => return self.advanceSingle(.r_paren, start),
            '{' => return self.advanceSingle(.l_brace, start),
            '}' => return self.advanceSingle(.r_brace, start),
            '[' => return self.advanceSingle(.l_bracket, start),
            ']' => return self.advanceSingle(.r_bracket, start),
            ':' => return self.advanceSingle(.colon, start),
            ',' => return self.advanceSingle(.comma, start),
            '@' => return self.advanceSingle(.at, start),
            '.' => return self.advanceSingle(.dot, start),
            '=' => return self.advanceSingle(.equal, start),
            ';' => return self.advanceSingle(.semicolon, start),
            else => {},
        }

        if (isIdentifierStart(c)) {
            return self.scanIdentifier();
        }

        if (std.ascii.isDigit(c)) {
            return self.scanNumber();
        }

        self.index += 1;
        return makeToken(.invalid, start, self.index);
    }

    fn skipTrivia(self: *Tokenizer) void {
        while (self.index < self.buffer.len) {
            const c = self.buffer[self.index];
            switch (c) {
                ' ', '\t', '\n', '\r' => {
                    self.index += 1;
                },
                '/' => {
                    if (self.skipLineComment()) continue;
                    return;
                },
                else => return,
            }
        }
    }

    fn skipLineComment(self: *Tokenizer) bool {
        if (self.index + 1 >= self.buffer.len) return false;
        if (self.buffer[self.index + 1] != '/') return false;
        self.index += 2;
        while (self.index < self.buffer.len and self.buffer[self.index] != '\n') {
            self.index += 1;
        }
        return true;
    }

    fn advanceSingle(self: *Tokenizer, tag: Token.Tag, start: usize) Token {
        self.index += 1;
        return makeToken(tag, start, self.index);
    }

    fn scanIdentifier(self: *Tokenizer) Token {
        const start = self.index;
        self.index += 1;
        while (self.index < self.buffer.len) {
            const ch = self.buffer[self.index];
            if (!isIdentifierContinue(ch)) break;
            self.index += 1;
        }
        const loc = Token.Loc{ .start = start, .end = self.index };
        const ident_bytes = self.buffer[loc.start..loc.end];
        if (Token.getKeyword(ident_bytes)) |tag| {
            return Token{ .tag = tag, .loc = loc };
        }
        return Token{ .tag = .identifier, .loc = loc };
    }

    fn scanNumber(self: *Tokenizer) Token {
        const start = self.index;
        self.index += 1;
        while (self.index < self.buffer.len and std.ascii.isDigit(self.buffer[self.index])) {
            self.index += 1;
        }
        return makeToken(.number_literal, start, self.index);
    }

    fn makeToken(tag: Token.Tag, start: usize, end: usize) Token {
        return .{
            .tag = tag,
            .loc = .{ .start = start, .end = end },
        };
    }
};

inline fn isIdentifierStart(c: u8) bool {
    return std.ascii.isAlphabetic(c) or c == '_';
}

inline fn isIdentifierContinue(c: u8) bool {
    return std.ascii.isAlphanumeric(c) or c == '_';
}
