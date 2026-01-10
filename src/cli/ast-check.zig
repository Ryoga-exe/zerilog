const std = @import("std");
const zerilog = @import("zerilog");
const clap = @import("clap");

pub fn astCheck(gpa: std.mem.Allocator, iter: *std.process.ArgIterator) !void {
    var diag = clap.Diagnostic{};
    var res = clap.parseEx(clap.Help, &params, parsers, iter, .{
        .diagnostic = &diag,
        .allocator = gpa,
    }) catch |err| {
        try diag.reportToFile(.stderr(), err);
        return err;
    };
    defer res.deinit();

    if (res.args.help != 0) {
        return clap.helpToFile(.stderr(), clap.Help, &params, .{});
    }

    const file_path = res.positionals[0] orelse return error.MissingFile;

    var file = try std.fs.cwd().openFile(file_path, .{});
    defer file.close();

    const source = try file.readToEndAlloc(gpa, 1024 * 1024);
    defer gpa.free(source);

    var source_buf = try gpa.alloc(u8, source.len + 1);
    defer gpa.free(source_buf);
    std.mem.copyForwards(u8, source_buf[0..source.len], source);
    source_buf[source.len] = 0;

    const ast = try zerilog.parse(gpa, source_buf[0..source.len :0]);
    defer {
        var ast_mut = ast;
        ast_mut.deinit(gpa);
    }

    var stderr_buffer: [1024]u8 = undefined;
    var stderr_writer = std.fs.File.stderr().writer(&stderr_buffer);
    const stderr = &stderr_writer.interface;

    var stdout_buffer: [1024]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    if (ast.errors.len > 0) {
        try dumpErrors(stderr, ast);
        try stderr.flush();
        return error.ParseFailed;
    }

    try dumpAst(stdout, ast);
    try stdout.flush();
}

const params = clap.parseParamsComptime(
    \\-h, --help  Display this help and exit.
    \\<FILE>
);

const parsers = .{
    .FILE = clap.parsers.string,
};

fn dumpErrors(writer: anytype, ast: zerilog.Ast) !void {
    for (ast.errors) |err| {
        const loc = ast.tokenLocation(err.token);
        const token_tag = ast.tokenTag(err.token);
        const token_text = ast.tokenSlice(err.token);
        const caret_offset: usize = if (err.token_is_prev) token_text.len else 0;
        if (err.tag == .expected_token) {
            const expected = err.extra.expected_tag.symbol();
            try writer.print(
                "error: expected {s} at {d}:{d} (found {s} '{s}')\n",
                .{ expected, loc.line, loc.column, token_tag.symbol(), token_text },
            );
        } else {
            const message = errorMessage(err);
            try writer.print(
                "error: {s} at {d}:{d} (found {s} '{s}')\n",
                .{ message, loc.line, loc.column, token_tag.symbol(), token_text },
            );
        }
        const line = ast.source[loc.line_start..loc.line_end];
        try writer.print("  {s}\n", .{line});
        try writer.writeAll("  ");
        try writeSpaces(writer, loc.column - 1 + caret_offset);
        try writer.writeAll("^\n");
    }
}

fn errorMessage(err: zerilog.AstError) []const u8 {
    return switch (err.tag) {
        .expected_token => "expected token",
        .expected_top_level => "expected top-level declaration",
        .expected_statement => "expected statement",
        .expected_expression => "expected expression",
        .expected_direction => "expected port direction",
        .expected_assignment_op => "expected assignment operator",
        .expected_module_after_pub => "expected 'module' after 'pub'",
    };
}

fn dumpAst(writer: anytype, ast: zerilog.Ast) !void {
    try dumpNode(writer, ast, ast.root, 0);
}

fn dumpNode(writer: anytype, ast: zerilog.Ast, node_index: zerilog.AstNode.Index, indent: usize) !void {
    const idx: usize = @intFromEnum(node_index);
    const node = ast.nodes.get(idx);
    const tag_name = @tagName(node.tag);
    const token_lexeme = ast.tokenSlice(node.main_token);

    try writeIndent(writer, indent);
    try writer.print("{d}: {s}", .{ @intFromEnum(node_index), tag_name });

    switch (node.tag) {
        .identifier, .number_literal, .builtin_ref, .enum_literal => {
            try writer.print(" {s}", .{token_lexeme});
        },
        .const_decl, .var_decl => {
            const name = ast.tokenSlice(@intCast(node.data.lhs));
            try writer.print(" {s}", .{name});
        },
        .module_decl, .pub_module_decl, .port => {
            try writer.print(" {s}", .{token_lexeme});
        },
        .assign, .binary => {
            if (token_lexeme.len != 0) {
                try writer.print(" {s}", .{token_lexeme});
            }
        },
        .field_access => {
            const name = ast.tokenSlice(@intCast(node.data.rhs));
            try writer.print(" .{s}", .{name});
        },
        else => {},
    }

    try writer.writeAll("\n");

    switch (node.tag) {
        .root => {
            for (ast.listSlice(node.data.lhs)) |child| {
                try dumpNode(writer, ast, @enumFromInt(child), indent + 1);
            }
        },
        .const_decl => try dumpNode(writer, ast, @enumFromInt(node.data.rhs), indent + 1),
        .var_decl => try dumpNode(writer, ast, @enumFromInt(node.data.rhs), indent + 1),
        .module_decl, .pub_module_decl => {
            for (ast.listSlice(node.data.lhs)) |child| {
                try dumpNode(writer, ast, @enumFromInt(child), indent + 1);
            }
            try dumpNode(writer, ast, @enumFromInt(node.data.rhs), indent + 1);
        },
        .port => try dumpNode(writer, ast, @enumFromInt(node.data.rhs), indent + 1),
        .block => {
            for (ast.listSlice(node.data.lhs)) |child| {
                try dumpNode(writer, ast, @enumFromInt(child), indent + 1);
            }
        },
        .always_ff, .comb => try dumpNode(writer, ast, @enumFromInt(node.data.lhs), indent + 1),
        .if_stmt => {
            try dumpNode(writer, ast, @enumFromInt(node.data.lhs), indent + 1);
            const branches = ast.listSlice(node.data.rhs);
            if (branches.len > 0) try dumpNode(writer, ast, @enumFromInt(branches[0]), indent + 1);
            if (branches.len > 1) {
                const opt: zerilog.AstNode.OptionalIndex = @enumFromInt(branches[1]);
                if (opt.unwrap()) |else_index| {
                    try dumpNode(writer, ast, else_index, indent + 1);
                }
            }
        },
        .if_reset => {
            try dumpNode(writer, ast, @enumFromInt(node.data.lhs), indent + 1);
            const opt: zerilog.AstNode.OptionalIndex = @enumFromInt(node.data.rhs);
            if (opt.unwrap()) |else_index| {
                try dumpNode(writer, ast, else_index, indent + 1);
            }
        },
        .assign, .binary => {
            try dumpNode(writer, ast, @enumFromInt(node.data.lhs), indent + 1);
            try dumpNode(writer, ast, @enumFromInt(node.data.rhs), indent + 1);
        },
        .call => {
            try dumpNode(writer, ast, @enumFromInt(node.data.lhs), indent + 1);
            for (ast.listSlice(node.data.rhs)) |child| {
                try dumpNode(writer, ast, @enumFromInt(child), indent + 1);
            }
        },
        .field_access => try dumpNode(writer, ast, @enumFromInt(node.data.lhs), indent + 1),
        .@"comptime" => try dumpNode(writer, ast, @enumFromInt(node.data.lhs), indent + 1),
        else => {},
    }
}

fn writeIndent(writer: anytype, indent: usize) !void {
    var i: usize = 0;
    while (i < indent) : (i += 1) {
        try writer.writeAll("  ");
    }
}

fn writeSpaces(writer: anytype, count: usize) !void {
    var i: usize = 0;
    while (i < count) : (i += 1) {
        try writer.writeAll(" ");
    }
}
