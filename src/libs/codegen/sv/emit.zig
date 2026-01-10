const std = @import("std");
const analyze = @import("../../analyze/analyze.zig");
const ir = @import("../../ir/ir.zig");

pub const Error = struct {
    module: []const u8,
    message: []const u8,
};

pub const Output = struct {
    sv: []const u8,
    errors: []const Error,

    pub fn deinit(self: *Output, allocator: std.mem.Allocator) void {
        allocator.free(self.sv);
        allocator.free(self.errors);
        self.* = undefined;
    }
};

pub fn emitSv(allocator: std.mem.Allocator, analysis: *analyze.Analysis, tree: ir.Ir) !Output {
    var buffer = std.ArrayList(u8).empty;
    errdefer buffer.deinit(allocator);
    var errors = std.ArrayList(Error).empty;
    errdefer errors.deinit(allocator);

    var writer = buffer.writer(allocator);
    for (tree.modules, 0..) |module, i| {
        if (i != 0) {
            try writer.writeAll("\n");
        }
        try emitModule(&writer, allocator, analysis, module, &errors);
    }

    return Output{
        .sv = try buffer.toOwnedSlice(allocator),
        .errors = try errors.toOwnedSlice(allocator),
    };
}

const ClockInfo = struct {
    name: []const u8,
    edge: analyze.ClockKind,
};

const ResetInfo = struct {
    name: []const u8,
    kind: analyze.ResetKind,
};

const AlwaysFf = @TypeOf((ir.Process{ .always_ff = undefined }).always_ff);
const AlwaysComb = @TypeOf((ir.Process{ .comb = undefined }).comb);

fn emitModule(
    writer: anytype,
    allocator: std.mem.Allocator,
    analysis: *analyze.Analysis,
    module: ir.Module,
    errors: *std.ArrayList(Error),
) anyerror!void {
    const clock_info = findClock(analysis, module);
    const reset_info = findReset(analysis, module);

    if (module.ports.len == 0) {
        try writer.print("module {s};\n", .{module.name});
    } else {
        try writer.print("module {s} (\n", .{module.name});
        for (module.ports, 0..) |port, idx| {
            try writeIndent(writer, 1);
            try writer.print("{s} ", .{dirName(port.dir)});
            try writeTypeSv(writer, analysis, port.ty);
            try writer.print(" {s}", .{port.name});
            if (idx + 1 < module.ports.len) {
                try writer.writeAll(",");
            }
            try writer.writeAll("\n");
        }
        try writer.writeAll(");\n");
    }

    for (module.vars) |v| {
        try writeIndent(writer, 1);
        try writeTypeSv(writer, analysis, v.ty);
        try writer.print(" {s};\n", .{v.name});
    }

    for (module.processes) |proc| {
        switch (proc) {
            .always_ff => |ff| {
                if (clock_info == null) {
                    try errors.append(allocator, .{ .module = module.name, .message = "missing clock for always_ff" });
                    continue;
                }
                if (ff.reset_body.start != ff.reset_body.end and reset_info == null) {
                    try errors.append(allocator, .{ .module = module.name, .message = "missing reset for if_reset" });
                    continue;
                }
                try emitAlwaysFf(writer, analysis, module, ff, clock_info.?, reset_info);
            },
            .comb => |c| {
                try emitAlwaysComb(writer, analysis, module, c);
            },
        }
    }

    try writer.writeAll("endmodule\n");
}

fn emitAlwaysFf(
    writer: anytype,
    analysis: *analyze.Analysis,
    module: ir.Module,
    ff: AlwaysFf,
    clock: ClockInfo,
    reset: ?ResetInfo,
) anyerror!void {
    try writeIndent(writer, 1);
    try writer.writeAll("always_ff @(");
    try writer.writeAll(if (clock.edge == .negedge) "negedge " else "posedge ");
    try writer.writeAll(clock.name);
    if (reset) |rst| {
        switch (rst.kind) {
            .async_low => try writer.print(" or negedge {s}", .{rst.name}),
            .async_high => try writer.print(" or posedge {s}", .{rst.name}),
            .sync_low, .sync_high => {},
        }
    }
    try writer.writeAll(") begin\n");

    if (reset) |rst| {
        const has_reset = ff.reset_body.start != ff.reset_body.end;
        if (has_reset) {
            try writeIndent(writer, 2);
            try writer.writeAll("if (");
            try writeResetCond(writer, rst);
            try writer.writeAll(") begin\n");
            try emitStmtRange(writer, analysis, module, ff.reset_body, .seq, 3);
            try writeIndent(writer, 2);
            if (ff.body.start != ff.body.end) {
                try writer.writeAll("end else begin\n");
                try emitStmtRange(writer, analysis, module, ff.body, .seq, 3);
                try writeIndent(writer, 2);
                try writer.writeAll("end\n");
            } else {
                try writer.writeAll("end\n");
            }
        } else {
            try emitStmtRange(writer, analysis, module, ff.body, .seq, 2);
        }
    } else {
        try emitStmtRange(writer, analysis, module, ff.body, .seq, 2);
    }

    try writeIndent(writer, 1);
    try writer.writeAll("end\n");
}

fn emitAlwaysComb(writer: anytype, analysis: *analyze.Analysis, module: ir.Module, comb: AlwaysComb) anyerror!void {
    try writeIndent(writer, 1);
    try writer.writeAll("always_comb begin\n");
    try emitStmtRange(writer, analysis, module, comb.body, .comb, 2);
    try writeIndent(writer, 1);
    try writer.writeAll("end\n");
}

const StmtContext = enum { seq, comb };

fn emitStmtRange(
    writer: anytype,
    analysis: *analyze.Analysis,
    module: ir.Module,
    range: ir.Range,
    context: StmtContext,
    indent: usize,
) anyerror!void {
    var i: u32 = range.start;
    while (i < range.end) : (i += 1) {
        try emitStmt(writer, analysis, module, module.stmts[i], context, indent);
    }
}

fn emitStmt(
    writer: anytype,
    analysis: *analyze.Analysis,
    module: ir.Module,
    stmt: ir.Stmt,
    context: StmtContext,
    indent: usize,
) anyerror!void {
    switch (stmt.tag) {
        .assign => {
            try writeIndent(writer, indent);
            const op = switch (context) { .seq => "<=", .comb => "=" };
            switch (stmt.data.assign.op) {
                .assign => {
                    try writer.print("{s} {s} ", .{ stmt.data.assign.target, op });
                    try emitExpr(writer, module, stmt.data.assign.value);
                    try writer.writeAll(";\n");
                },
                .add_assign => {
                    try writer.print("{s} {s} {s} + ", .{ stmt.data.assign.target, op, stmt.data.assign.target });
                    try emitExpr(writer, module, stmt.data.assign.value);
                    try writer.writeAll(";\n");
                },
            }
        },
        .if_stmt => {
            try writeIndent(writer, indent);
            try writer.writeAll("if (");
            try emitExpr(writer, module, stmt.data.if_stmt.cond);
            try writer.writeAll(") begin\n");
            try emitStmtRange(writer, analysis, module, stmt.data.if_stmt.then_body, context, indent + 1);
            if (stmt.data.if_stmt.else_body.start != stmt.data.if_stmt.else_body.end) {
                try writeIndent(writer, indent);
                try writer.writeAll("end else begin\n");
                try emitStmtRange(writer, analysis, module, stmt.data.if_stmt.else_body, context, indent + 1);
            }
            try writeIndent(writer, indent);
            try writer.writeAll("end\n");
        },
    }
}

fn emitExpr(writer: anytype, module: ir.Module, expr_id: ir.ExprId) anyerror!void {
    const expr = module.exprs[expr_id];
    switch (expr.tag) {
        .ident => try writer.writeAll(expr.data.ident),
        .number => try writer.print("{d}", .{expr.data.number}),
        .binary => {
            try writer.writeAll("(");
            try emitExpr(writer, module, expr.data.binary.lhs);
            const op = switch (expr.data.binary.op) { .add => "+", .equal_equal => "==" };
            try writer.print(" {s} ", .{op});
            try emitExpr(writer, module, expr.data.binary.rhs);
            try writer.writeAll(")");
        },
    }
}

fn writeTypeSv(writer: anytype, analysis: *analyze.Analysis, id: analyze.TypeId) anyerror!void {
    const ty = analysis.types.get(id);
    switch (ty) {
        .clock, .reset, .anyclock, .anyreset => try writer.writeAll("logic"),
        .bit => |info| try writeVectorType(writer, "bit", info.signedness, info.bits),
        .logic => |info| try writeVectorType(writer, "logic", info.signedness, info.bits),
    }
}

fn writeVectorType(writer: anytype, base: []const u8, signedness: analyze.Signedness, bits: u32) anyerror!void {
    try writer.writeAll(base);
    if (signedness == .signed) {
        try writer.writeAll(" signed");
    }
    if (bits > 1) {
        try writer.print(" [{d}:0]", .{bits - 1});
    }
}

fn findClock(analysis: *analyze.Analysis, module: ir.Module) ?ClockInfo {
    for (module.ports) |port| {
        switch (analysis.types.get(port.ty)) {
            .clock => |edge| return .{ .name = port.name, .edge = edge },
            .anyclock => return .{ .name = port.name, .edge = .posedge },
            else => {},
        }
    }
    return null;
}

fn findReset(analysis: *analyze.Analysis, module: ir.Module) ?ResetInfo {
    for (module.ports) |port| {
        switch (analysis.types.get(port.ty)) {
            .reset => |kind| return .{ .name = port.name, .kind = kind },
            .anyreset => return .{ .name = port.name, .kind = .async_low },
            else => {},
        }
    }
    return null;
}

fn writeResetCond(writer: anytype, reset: ResetInfo) anyerror!void {
    switch (reset.kind) {
        .async_low, .sync_low => {
            try writer.writeAll("!");
            try writer.writeAll(reset.name);
        },
        .async_high, .sync_high => try writer.writeAll(reset.name),
    }
}

fn dirName(dir: ir.Direction) []const u8 {
    return switch (dir) {
        .input => "input",
        .output => "output",
        .inout => "inout",
    };
}

fn writeIndent(writer: anytype, level: usize) anyerror!void {
    var i: usize = 0;
    while (i < level * 2) : (i += 1) {
        try writer.writeAll(" ");
    }
}
