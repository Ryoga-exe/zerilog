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
        const token = ast.tokens[@intCast(err.token)];
        const pos = lineCol(ast.source, token.loc.start);
        const found = token.tag.symbol();
        try writer.print(
            "error: {s} at {d}:{d} (found {s})\n",
            .{ err.message, pos.line, pos.column, found },
        );
    }
}

fn dumpAst(writer: anytype, ast: zerilog.Ast) !void {
    try dumpNode(writer, ast, ast.root, 0);
}

fn dumpNode(writer: anytype, ast: zerilog.Ast, node_index: zerilog.AstNode.Index, indent: usize) !void {
    if (node_index == zerilog.AstNull) {
        try writeIndent(writer, indent);
        try writer.writeAll("<null>\n");
        return;
    }

    const node = ast.nodes[@intCast(node_index)];
    const tag_name = @tagName(node.tag);
    const token_lexeme = ast.tokenSlice(node.main_token);

    try writeIndent(writer, indent);
    try writer.print("{d}: {s}", .{ node_index, tag_name });

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
                try dumpNode(writer, ast, child, indent + 1);
            }
        },
        .const_decl => try dumpNode(writer, ast, @intCast(node.data.rhs), indent + 1),
        .var_decl => try dumpNode(writer, ast, @intCast(node.data.rhs), indent + 1),
        .module_decl, .pub_module_decl => {
            for (ast.listSlice(node.data.lhs)) |child| {
                try dumpNode(writer, ast, child, indent + 1);
            }
            try dumpNode(writer, ast, @intCast(node.data.rhs), indent + 1);
        },
        .port => try dumpNode(writer, ast, @intCast(node.data.rhs), indent + 1),
        .block => {
            for (ast.listSlice(node.data.lhs)) |child| {
                try dumpNode(writer, ast, child, indent + 1);
            }
        },
        .always_ff, .comb => try dumpNode(writer, ast, @intCast(node.data.lhs), indent + 1),
        .if_stmt => {
            try dumpNode(writer, ast, @intCast(node.data.lhs), indent + 1);
            const branches = ast.listSlice(node.data.rhs);
            if (branches.len > 0) try dumpNode(writer, ast, branches[0], indent + 1);
            if (branches.len > 1) try dumpNode(writer, ast, branches[1], indent + 1);
        },
        .if_reset => {
            try dumpNode(writer, ast, @intCast(node.data.lhs), indent + 1);
            if (node.data.rhs != zerilog.AstNull) {
                try dumpNode(writer, ast, @intCast(node.data.rhs), indent + 1);
            }
        },
        .assign, .binary => {
            try dumpNode(writer, ast, @intCast(node.data.lhs), indent + 1);
            try dumpNode(writer, ast, @intCast(node.data.rhs), indent + 1);
        },
        .call => {
            try dumpNode(writer, ast, @intCast(node.data.lhs), indent + 1);
            for (ast.listSlice(node.data.rhs)) |child| {
                try dumpNode(writer, ast, child, indent + 1);
            }
        },
        .field_access => try dumpNode(writer, ast, @intCast(node.data.lhs), indent + 1),
        .@"comptime" => try dumpNode(writer, ast, @intCast(node.data.lhs), indent + 1),
        else => {},
    }
}

fn writeIndent(writer: anytype, indent: usize) !void {
    var i: usize = 0;
    while (i < indent) : (i += 1) {
        try writer.writeAll("  ");
    }
}

const LineCol = struct {
    line: usize,
    column: usize,
};

fn lineCol(source: []const u8, index: usize) LineCol {
    var line: usize = 1;
    var col: usize = 1;
    var i: usize = 0;
    while (i < index and i < source.len) : (i += 1) {
        if (source[i] == '\n') {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
    }
    return .{ .line = line, .column = col };
}
