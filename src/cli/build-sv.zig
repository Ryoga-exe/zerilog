const std = @import("std");
const zerilog = @import("zerilog");
const clap = @import("clap");

pub fn buildSv(gpa: std.mem.Allocator, iter: *std.process.ArgIterator) !void {
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

    if (ast.errors.len > 0) {
        try dumpParseErrors(stderr, ast);
        try stderr.flush();
        return error.ParseFailed;
    }

    var analysis = try zerilog.analyze(gpa, &ast);
    defer analysis.deinit(gpa);

    if (analysis.errors.len > 0) {
        try dumpAnalyzeErrors(stderr, ast, analysis.errors);
        try stderr.flush();
        return error.AnalyzeFailed;
    }

    var lowered = try zerilog.lower(gpa, &ast, &analysis);
    defer lowered.deinit(gpa);

    if (lowered.errors.len > 0) {
        try dumpLowerErrors(stderr, ast, lowered.errors);
        try stderr.flush();
        return error.LowerFailed;
    }

    var output = try zerilog.emitSv(gpa, &analysis, lowered.ir);
    defer output.deinit(gpa);

    if (output.errors.len > 0) {
        try dumpEmitErrors(stderr, output.errors);
        try stderr.flush();
        return error.EmitFailed;
    }

    if (res.args.output) |out_path| {
        var out_file = try std.fs.cwd().createFile(out_path, .{ .truncate = true });
        defer out_file.close();
        try out_file.writeAll(output.sv);
    } else {
        var stdout_buffer: [1024]u8 = undefined;
        var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
        const stdout = &stdout_writer.interface;
        try stdout.writeAll(output.sv);
        try stdout.flush();
    }
}

const params = clap.parseParamsComptime(
    \\-h, --help           Display this help and exit.
    \\-o, --output <FILE>  Write output to FILE.
    \\<FILE>
);

const parsers = .{
    .FILE = clap.parsers.string,
    .output = clap.parsers.string,
};

fn dumpParseErrors(writer: anytype, ast: zerilog.Ast) !void {
    for (ast.errors) |err| {
        const loc = ast.tokenLocation(err.token);
        const token_tag = ast.tokenTag(err.token);
        const token_text = ast.tokenSlice(err.token);
        if (err.tag == .expected_token) {
            const expected = err.extra.expected_tag.symbol();
            try writer.print(
                "error: expected {s} at {d}:{d} (found {s} '{s}')\n",
                .{ expected, loc.line, loc.column, token_tag.symbol(), token_text },
            );
        } else {
            const message = parseErrorMessage(err.tag);
            try writer.print(
                "error: {s} at {d}:{d} (found {s} '{s}')\n",
                .{ message, loc.line, loc.column, token_tag.symbol(), token_text },
            );
        }
        const line = ast.source[loc.line_start..loc.line_end];
        try writer.print("  {s}\n", .{line});
        try writer.writeAll("  ");
        try writeSpaces(writer, loc.column - 1);
        try writer.writeAll("^\n");
    }
}

fn parseErrorMessage(tag: zerilog.AstError.Tag) []const u8 {
    return switch (tag) {
        .expected_token => "expected token",
        .expected_top_level => "expected top-level declaration",
        .expected_statement => "expected statement",
        .expected_expression => "expected expression",
        .expected_direction => "expected port direction",
        .expected_assignment_op => "expected assignment operator",
        .expected_module_after_pub => "expected 'module' after 'pub'",
    };
}

fn dumpAnalyzeErrors(writer: anytype, ast: zerilog.Ast, errors: []const zerilog.AnalyzeError) !void {
    for (errors) |err| {
        const loc = ast.tokenLocation(err.token);
        try writer.print("error: {s} at {d}:{d}\n", .{ err.message, loc.line, loc.column });
        const line = ast.source[loc.line_start..loc.line_end];
        try writer.print("  {s}\n", .{line});
        try writer.writeAll("  ");
        try writeSpaces(writer, loc.column - 1);
        try writer.writeAll("^\n");
    }
}

fn dumpLowerErrors(writer: anytype, ast: zerilog.Ast, errors: []const zerilog.LowerError) !void {
    for (errors) |err| {
        const loc = ast.tokenLocation(err.token);
        try writer.print("error: {s} at {d}:{d}\n", .{ err.message, loc.line, loc.column });
        const line = ast.source[loc.line_start..loc.line_end];
        try writer.print("  {s}\n", .{line});
        try writer.writeAll("  ");
        try writeSpaces(writer, loc.column - 1);
        try writer.writeAll("^\n");
    }
}

fn dumpEmitErrors(writer: anytype, errors: []const zerilog.EmitSvError) !void {
    for (errors) |err| {
        try writer.print("error: {s} (module {s})\n", .{ err.message, err.module });
    }
}

fn writeSpaces(writer: anytype, count: usize) !void {
    var i: usize = 0;
    while (i < count) : (i += 1) {
        try writer.writeAll(" ");
    }
}
