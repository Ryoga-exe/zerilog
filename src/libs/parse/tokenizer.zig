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
        .{ "else", .keyword_else },
    });

    pub fn getKeyword(bytes: []const u8) ?Tag {
        return keywords.get(bytes);
    }

    pub const Tag = enum {
        invalid,
        identifier,
        eof,
        builtin,
        number_literal,
        doc_comment,
        bang,
        l_paren,
        r_paren,
        semicolon,
        l_brace,
        r_brace,
        l_bracket,
        r_bracket,
        keyword_module,
        keyword_pub,
        keyword_input,
        keyword_output,
        keyword_inout,
        keyword_var,
        keyword_else,

        pub fn lexeme(tag: Tag) ?[]const u8 {
            return switch (tag) {
                .invalid,
                .identifier,
                .eof,
                .builtin,
                => null,

                .l_paren => "(",
                .r_paren => ")",
                .semicolon => ";",
                .keyword_module => "module",
                .keyword_pub => "pub",
                .keyword_else => "else",
            };
        }

        pub fn symbol(tag: Tag) []const u8 {
            return tag.lexeme() orelse switch (tag) {
                .invalid => "invalid token",
                .eof => "EOF",
                else => unreachable,
            };
        }
    };
};

pub const Tokenizer = struct {
    buffer: [:0]const u8,
    index: usize,

    /// For debugging purposes.
    pub fn dump(self: *Tokenizer, token: *const Token) void {
        std.debug.print("{s} \"{s}\"\n", .{ @tagName(token.tag), self.buffer[token.loc.start..token.loc.end] });
    }

    pub fn init(buffer: [:0]const u8) Tokenizer {
        // Skip the UTF-8 BOM if present.
        return .{
            .buffer = buffer,
            .index = if (std.mem.startsWith(u8, buffer, "\xEF\xBB\xBF")) 3 else 0,
        };
    }

    const State = enum {
        start,
        expect_newline,
        identifier,
        builtin,
        equal,
        bang,
        pipe,
        minus,
        asterisk,
        slash,
        line_comment_start,
        line_comment,
        doc_comment_start,
        doc_comment,
        int,
        saw_at_sign,
        invalid,
    };

    /// After this returns invalid, it will reset on the next newline, returning tokens starting from there.
    /// An eof token will always be returned at the end.
    pub fn next(self: *Tokenizer) Token {
        var result: Token = .{
            .tag = undefined,
            .loc = .{
                .start = self.index,
                .end = undefined,
            },
        };
        state: switch (State.start) {
            .start => switch (self.buffer[self.index]) {
                0 => {
                    if (self.index == self.buffer.len) {
                        return .{
                            .tag = .eof,
                            .loc = .{
                                .start = self.index,
                                .end = self.index,
                            },
                        };
                    } else {
                        continue :state .invalid;
                    }
                },
                ' ', '\n', '\t', '\r' => {
                    self.index += 1;
                    result.loc.start = self.index;
                    continue :state .start;
                },
                // TODO: '"'
                // TODO: '\''
                'a'...'z', 'A'...'Z', '_' => {
                    result.tag = .identifier;
                    continue :state .identifier;
                },
                '@' => continue :state .saw_at_sign,
                '=' => continue :state .equal,
                '!' => continue :state .bang,
                '|' => continue :state .pipe,
                '(' => {
                    result.tag = .l_paren;
                    self.index += 1;
                },
                ')' => {
                    result.tag = .r_paren;
                    self.index += 1;
                },
                '[' => {
                    result.tag = .l_bracket;
                    self.index += 1;
                },
                ']' => {
                    result.tag = .r_bracket;
                    self.index += 1;
                },
                '{' => {
                    result.tag = .l_brace;
                    self.index += 1;
                },
                '}' => {
                    result.tag = .r_brace;
                    self.index += 1;
                },
                ';' => {
                    result.tag = .semicolon;
                    self.index += 1;
                },
                // TODO: '%'
                // TODO: '*'
                // TODO: '+'
                // TODO: '<'
                // TODO: '>'
                // TODO: '^'
                // TODO: '\\'
                // TODO: '~'
                // TODO: '.'
                '-' => continue :state .minus,
                '/' => continue :state .slash,
                // TODO: '&'
                '0'...'9' => {
                    result.tag = .number_literal;
                    self.index += 1;
                    continue :state .int;
                },
                else => continue :state .invalid,
            },
            .expect_newline => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    0 => {
                        if (self.index == self.buffer.len) {
                            result.tag = .invalid;
                        } else {
                            continue :state .invalid;
                        }
                    },
                    '\n' => {
                        self.index += 1;
                        result.loc.start = self.index;
                        continue :state .start;
                    },
                    else => continue :state .invalid,
                }
            },
            .invalid => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    0 => if (self.index == self.buffer.len) {
                        result.tag = .invalid;
                    } else {
                        continue :state .invalid;
                    },
                    '\n' => result.tag = .invalid,
                    else => continue :state .invalid,
                }
            },
            .saw_at_sign => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    0, '\n' => result.tag = .invalid,
                    // TODO: '"'
                    'a'...'z', 'A'...'Z', '_' => {
                        result.tag = .builtin;
                        continue :state .builtin;
                    },
                    else => continue :state .invalid,
                }
            },
            // TODO: .ampersand
            .asterisk => {
                self.index += 1;
                // TODO: switch (self.buffer[self.index])
            },
            .identifier => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    'a'...'z', 'A'...'Z', '_', '0'...'9' => continue :state .identifier,
                    else => {
                        const ident = self.buffer[result.loc.start..self.index];
                        if (Token.getKeyword(ident)) |tag| {
                            result.tag = tag;
                        }
                    },
                }
            },
            .builtin => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    'a'...'z', 'A'...'Z', '_', '0'...'9' => continue :state .builtin,
                    else => {},
                }
            },
            // TODO: .backslash
            .bang => {
                self.index += 1;
                // TODO: switch (self.buffer[self.index])
            },
            .pipe => {
                self.index += 1;
                // TODO: switch (self.buffer[self.index])
            },
            .equal => {
                self.index += 1;
                // TODO: switch (self.buffer[self.index])
            },
            .minus => {
                self.index += 1;
                // TODO: switch (self.buffer[self.index])
            },
            .slash => {
                self.index += 1;
                // TODO: switch (self.buffer[self.index])
            },
            .line_comment_start => {
                self.index += 1;
                // TODO: switch (self.buffer[self.index])
            },
            .doc_comment_start => {
                self.index += 1;
                // TODO: switch (self.buffer[self.index])
            },
            .line_comment => {
                self.index += 1;
                // TODO: switch (self.buffer[self.index])
            },
            .doc_comment => {
                self.index += 1;
                // TODO: switch (self.buffer[self.index])
            },
            .int => switch (self.buffer[self.index]) {
                // TODO: '.'
                '_', 'a'...'d', 'f'...'o', 'q'...'z', 'A'...'D', 'F'...'O', 'Q'...'Z', '0'...'9' => {
                    self.index += 1;
                    continue :state .int;
                },
                // TODO: 'e', 'E', 'p', 'P'
                else => {},
            },
        }

        result.loc.end = self.index;
        return result;
    }
};

test "keywords" {
    try testTokenize("pub module else", &.{ .keyword_pub, .keyword_module, .keyword_else });
}

fn testTokenize(source: [:0]const u8, expected_token_tags: []const Token.Tag) !void {
    var tokenizer = Tokenizer.init(source);
    for (expected_token_tags) |expected_token_tag| {
        const token = tokenizer.next();
        try std.testing.expectEqual(expected_token_tag, token.tag);
    }
    // Last token should always be eof, even when the last token was invalid,
    // in which case the tokenizer is in an invalid state, which can only be
    // recovered by opinionated means outside the scope of this implementation.
    const last_token = tokenizer.next();
    try std.testing.expectEqual(Token.Tag.eof, last_token.tag);
    try std.testing.expectEqual(source.len, last_token.loc.start);
    try std.testing.expectEqual(source.len, last_token.loc.end);
}
