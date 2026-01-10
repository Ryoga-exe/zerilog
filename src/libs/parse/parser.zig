const std = @import("std");
const Ast = @import("Ast.zig");
const Parse = @import("Parse.zig");
const tokenizer = @import("tokenizer.zig");

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

    var parser = Parse.init(allocator, source, tokens.slice());
    defer parser.errors.deinit(allocator);
    defer parser.nodes.deinit(allocator);
    defer parser.extra_data.deinit(allocator);

    try parser.parseRoot();

    const extra_data = try parser.extra_data.toOwnedSlice(allocator);
    errdefer allocator.free(extra_data);
    const errors = try parser.errors.toOwnedSlice(allocator);
    errdefer allocator.free(errors);

    var nodes = parser.nodes;
    parser.nodes = Ast.NodeList.empty;

    return Ast.Ast{
        .source = source,
        .tokens = tokens.toOwnedSlice(),
        .nodes = nodes.toOwnedSlice(),
        .extra_data = extra_data,
        .errors = errors,
        .root = .root,
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
