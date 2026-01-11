const std = @import("std");
const zerilog = @import("zerilog");
const clap = @import("clap");

pub fn dumpIr(gpa: std.mem.Allocator, iter: *std.process.ArgIterator) !void {
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
        try dumpAstErrors(stderr, ast);
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

    var stdout_buffer: [1024]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;
    try dumpIrTree(stdout, &analysis, lowered.ir);
    try stdout.flush();
}

const params = clap.parseParamsComptime(
    \\-h, --help  Display this help and exit.
    \\<FILE>
);

const parsers = .{
    .FILE = clap.parsers.string,
};

fn dumpAstErrors(writer: anytype, ast: zerilog.Ast) !void {
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

fn dumpIrTree(writer: anytype, analysis: *zerilog.Analysis, ir: zerilog.Ir) anyerror!void {
    for (ir.modules) |module| {
        try writer.print("module {s}\n", .{module.name});
        if (module.ports.len > 0) {
            try writer.writeAll("  ports\n");
            for (module.ports) |port| {
                try writer.print("    {s} {s}: ", .{ dirName(port.dir), port.name });
                try writeType(writer, analysis, port.ty);
                try writer.writeAll("\n");
            }
        }
        if (module.vars.len > 0) {
            try writer.writeAll("  vars\n");
            for (module.vars) |v| {
                try writer.print("    {s}: ", .{v.name});
                try writeType(writer, analysis, v.ty);
                try writer.writeAll("\n");
            }
        }
        if (module.processes.len > 0) {
            try writer.writeAll("  processes\n");
            for (module.processes) |proc| {
                switch (proc) {
                    .always_ff => |ff| {
                        try writer.writeAll("    always_ff\n");
                        try writer.writeAll("      reset\n");
                        try dumpStmtRange(writer, module, ff.reset_body, 3);
                        try writer.writeAll("      body\n");
                        try dumpStmtRange(writer, module, ff.body, 3);
                    },
                    .comb => |c| {
                        try writer.writeAll("    comb\n");
                        try dumpStmtRange(writer, module, c.body, 3);
                    },
                }
            }
        }
    }
}

fn dumpStmtRange(writer: anytype, module: zerilog.IrModule, range: zerilog.IrRange, indent: usize) anyerror!void {
    var i: u32 = range.start;
    while (i < range.end) : (i += 1) {
        try dumpStmt(writer, module, i, indent);
    }
}

fn dumpStmt(writer: anytype, module: zerilog.IrModule, stmt_id: u32, indent: usize) anyerror!void {
    const stmt = module.stmts[stmt_id];
    try writeIndent(writer, indent);
    switch (stmt.tag) {
        .assign => {
            const op = switch (stmt.data.assign.op) { .assign => "=", .add_assign => "+=" };
            try writer.print("assign {s} {s} ", .{ stmt.data.assign.target, op });
            try dumpExpr(writer, module, stmt.data.assign.value);
            try writer.writeAll("\n");
        },
        .if_stmt => {
            try writer.writeAll("if ");
            try dumpExpr(writer, module, stmt.data.if_stmt.cond);
            try writer.writeAll("\n");
            try dumpStmtRange(writer, module, stmt.data.if_stmt.then_body, indent + 1);
            if (stmt.data.if_stmt.else_body.start != stmt.data.if_stmt.else_body.end) {
                try writeIndent(writer, indent);
                try writer.writeAll("else\n");
                try dumpStmtRange(writer, module, stmt.data.if_stmt.else_body, indent + 1);
            }
        },
    }
}

fn dumpExpr(writer: anytype, module: zerilog.IrModule, expr_id: zerilog.IrExprId) anyerror!void {
    const expr = module.exprs[expr_id];
    switch (expr.tag) {
        .ident => try writer.print("{s}", .{expr.data.ident}),
        .number => try writer.print("{d}", .{expr.data.number}),
        .binary => {
            try writer.writeAll("(");
            try dumpExpr(writer, module, expr.data.binary.lhs);
            const op = switch (expr.data.binary.op) { .add => "+", .equal_equal => "==" };
            try writer.print(" {s} ", .{op});
            try dumpExpr(writer, module, expr.data.binary.rhs);
            try writer.writeAll(")");
        },
    }
}

fn dirName(dir: zerilog.IrDirection) []const u8 {
    return switch (dir) {
        .input => "input",
        .output => "output",
        .inout => "inout",
    };
}

fn writeType(writer: anytype, analysis: *zerilog.Analysis, id: zerilog.IrTypeId) anyerror!void {
    const ty = analysis.types.get(id);
    switch (ty) {
        .clock => |k| try writer.writeAll(if (k == .posedge) "clock_posedge" else "clock_negedge"),
        .reset => |k| try writer.writeAll(switch (k) {
            .async_low => "reset_async_low",
            .async_high => "reset_async_high",
            .sync_low => "reset_sync_low",
            .sync_high => "reset_sync_high",
        }),
        .bit => |info| {
            try writer.writeAll(if (info.signedness == .signed) "sbit" else "bit");
            if (info.bits > 1) try writer.print("{d}", .{info.bits});
        },
        .logic => |info| {
            try writer.writeAll(if (info.signedness == .signed) "slogic" else "logic");
            if (info.bits > 1) try writer.print("{d}", .{info.bits});
        },
        .anyclock => try writer.writeAll("anyclock"),
        .anyreset => try writer.writeAll("anyreset"),
    }
}

fn writeSpaces(writer: anytype, count: usize) anyerror!void {
    var i: usize = 0;
    while (i < count) : (i += 1) {
        try writer.writeAll(" ");
    }
}

fn writeIndent(writer: anytype, indent: usize) anyerror!void {
    var i: usize = 0;
    while (i < indent) : (i += 1) {
        try writer.writeAll("  ");
    }
}
