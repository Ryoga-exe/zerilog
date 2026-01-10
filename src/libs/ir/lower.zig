const std = @import("std");
const Ast = @import("../parse/Ast.zig");
const analyze = @import("../analyze/analyze.zig");
const ir = @import("ir.zig");
const Token = @import("../parse/tokenizer.zig").Token;

pub const Error = struct {
    token: Ast.TokenIndex,
    message: []const u8,
};

pub const Lowered = struct {
    ir: ir.Ir,
    errors: []const Error,

    pub fn deinit(self: *Lowered, allocator: std.mem.Allocator) void {
        self.ir.deinit(allocator);
        allocator.free(self.errors);
        self.* = undefined;
    }
};

const Lowerer = struct {
    gpa: std.mem.Allocator,
    ast: *const Ast.Ast,
    analysis: *analyze.Analysis,
    errors: std.ArrayList(Error),

    fn init(gpa: std.mem.Allocator, ast: *const Ast.Ast, analysis: *analyze.Analysis) Lowerer {
        return .{
            .gpa = gpa,
            .ast = ast,
            .analysis = analysis,
            .errors = std.ArrayList(Error).empty,
        };
    }

    fn deinit(self: *Lowerer) void {
        self.errors.deinit(self.gpa);
    }

    fn addError(self: *Lowerer, token: Ast.TokenIndex, message: []const u8) !void {
        try self.errors.append(self.gpa, .{ .token = token, .message = message });
    }

    fn resolveType(self: *Lowerer, node: Ast.Node.Index) std.mem.Allocator.Error!?analyze.TypeId {
        var tmp = std.ArrayList(analyze.Error).empty;
        defer tmp.deinit(self.gpa);
        const result = try analyze.resolveTypeExpr(self.gpa, self.ast, self.analysis, node, &tmp);
        if (tmp.items.len > 0) {
            for (tmp.items) |err| {
                try self.errors.append(self.gpa, .{ .token = err.token, .message = err.message });
            }
        }
        return result;
    }

    fn defaultType(self: *Lowerer) std.mem.Allocator.Error!analyze.TypeId {
        return self.analysis.types.getOrAdd(self.gpa, .{ .logic = .{ .signedness = .unsigned, .bits = 1 } });
    }

    fn lowerExpr(self: *Lowerer, exprs: *std.ArrayList(ir.Expr), node: Ast.Node.Index) std.mem.Allocator.Error!ir.ExprId {
        const tag = self.ast.nodeTag(node);
        const data = self.ast.nodeData(node);
        switch (tag) {
            .identifier => {
                const name = self.ast.tokenSlice(self.ast.nodeMainToken(node));
                return try pushExpr(exprs, .{ .tag = .ident, .data = .{ .ident = name } }, self.gpa);
            },
            .number_literal => {
                const text = self.ast.tokenSlice(self.ast.nodeMainToken(node));
                const value = std.fmt.parseUnsigned(u64, text, 10) catch 0;
                return try pushExpr(exprs, .{ .tag = .number, .data = .{ .number = value } }, self.gpa);
            },
            .binary => {
                const op_tag = self.ast.tokenTag(self.ast.nodeMainToken(node));
                const op: ir.BinaryOp = switch (op_tag) {
                    .plus => .add,
                    .equal_equal => .equal_equal,
                    else => blk: {
                        try self.addError(self.ast.nodeMainToken(node), "unsupported binary op");
                        break :blk .add;
                    },
                };
                const lhs = try self.lowerExpr(exprs, data.binary.lhs);
                const rhs = try self.lowerExpr(exprs, data.binary.rhs);
                return try pushExpr(exprs, .{ .tag = .binary, .data = .{ .binary = .{ .op = op, .lhs = lhs, .rhs = rhs } } }, self.gpa);
            },
            else => {
                try self.addError(self.ast.nodeMainToken(node), "unsupported expression");
                return try pushExpr(exprs, .{ .tag = .number, .data = .{ .number = 0 } }, self.gpa);
            },
        }
    }

    fn lowerStmt(self: *Lowerer, stmts: *std.ArrayList(ir.Stmt), exprs: *std.ArrayList(ir.Expr), node: Ast.Node.Index) std.mem.Allocator.Error!ir.Stmt {
        const tag = self.ast.nodeTag(node);
        const data = self.ast.nodeData(node);
        switch (tag) {
            .assign => {
                const op_tag = self.ast.tokenTag(self.ast.nodeMainToken(node));
                const op: ir.AssignOp = switch (op_tag) {
                    .plus_equal => .add_assign,
                    else => .assign,
                };
                const target_node = data.assign.target;
                const target_name = self.ast.tokenSlice(self.ast.nodeMainToken(target_node));
                const value = try self.lowerExpr(exprs, data.assign.value);
                return .{ .tag = .assign, .data = .{ .assign = .{ .target = target_name, .op = op, .value = value } } };
            },
            .if_stmt => {
                const cond = try self.lowerExpr(exprs, data.if_stmt.cond);
                const then_range = try self.lowerBlock(stmts, exprs, data.if_stmt.then_block);
                var else_range = ir.Range{ .start = then_range.end, .end = then_range.end };
                if (data.if_stmt.else_block.unwrap()) |else_block| {
                    else_range = try self.lowerBlock(stmts, exprs, else_block);
                }
                return .{ .tag = .if_stmt, .data = .{ .if_stmt = .{ .cond = cond, .then_body = then_range, .else_body = else_range } } };
            },
            else => {
                try self.addError(self.ast.nodeMainToken(node), "unsupported statement");
            },
        }
        return .{ .tag = .assign, .data = .{ .assign = .{ .target = "", .op = .assign, .value = 0 } } };
    }

    fn lowerBlock(self: *Lowerer, stmts: *std.ArrayList(ir.Stmt), exprs: *std.ArrayList(ir.Expr), node: Ast.Node.Index) std.mem.Allocator.Error!ir.Range {
        const data = self.ast.nodeData(node).block;
        var local = std.ArrayList(ir.Stmt).empty;
        defer local.deinit(self.gpa);
        for (self.ast.listSlice(data.statements)) |stmt_idx| {
            const stmt_node: Ast.Node.Index = @enumFromInt(stmt_idx);
            const stmt = try self.lowerStmt(stmts, exprs, stmt_node);
            try local.append(self.gpa, stmt);
        }
        const start = @as(u32, @intCast(stmts.items.len));
        try stmts.appendSlice(self.gpa, local.items);
        const end = @as(u32, @intCast(stmts.items.len));
        return .{ .start = start, .end = end };
    }

    fn lowerProcessAlways(self: *Lowerer, stmts: *std.ArrayList(ir.Stmt), exprs: *std.ArrayList(ir.Expr), node: Ast.Node.Index) std.mem.Allocator.Error!ir.Process {
        const block = self.ast.nodeData(node).unary;
        const data = self.ast.nodeData(block).block;
        const list = self.ast.listSlice(data.statements);
        const empty_range = ir.Range{ .start = @intCast(stmts.items.len), .end = @intCast(stmts.items.len) };
        var reset_range = empty_range;
        var body_range = empty_range;
        var start_index: usize = 0;
        if (list.len > 0) {
            const first: Ast.Node.Index = @enumFromInt(list[0]);
            if (self.ast.nodeTag(first) == .if_reset) {
                const if_data = self.ast.nodeData(first).if_reset;
                reset_range = try self.lowerBlock(stmts, exprs, if_data.then_block);
                if (if_data.else_block.unwrap()) |else_block| {
                    body_range = try self.lowerBlock(stmts, exprs, else_block);
                }
                start_index = 1;
            }
        }
        if (start_index < list.len) {
            const start = @as(u32, @intCast(stmts.items.len));
            var local = std.ArrayList(ir.Stmt).empty;
            defer local.deinit(self.gpa);
            for (list[start_index..]) |stmt_idx| {
                const stmt_node: Ast.Node.Index = @enumFromInt(stmt_idx);
                const stmt = try self.lowerStmt(stmts, exprs, stmt_node);
                try local.append(self.gpa, stmt);
            }
            try stmts.appendSlice(self.gpa, local.items);
            const end = @as(u32, @intCast(stmts.items.len));
            if (body_range.start == body_range.end) {
                body_range = .{ .start = start, .end = end };
            } else {
                body_range.end = end;
            }
        }
        return .{ .always_ff = .{ .reset_body = reset_range, .body = body_range } };
    }

    fn lowerProcessComb(self: *Lowerer, stmts: *std.ArrayList(ir.Stmt), exprs: *std.ArrayList(ir.Expr), node: Ast.Node.Index) std.mem.Allocator.Error!ir.Process {
        const block = self.ast.nodeData(node).unary;
        const body_range = try self.lowerBlock(stmts, exprs, block);
        return .{ .comb = .{ .body = body_range } };
    }

    fn lowerModule(self: *Lowerer, node: Ast.Node.Index) std.mem.Allocator.Error!ir.Module {
        const name = self.ast.tokenSlice(self.ast.nodeMainToken(node));
        const data = self.ast.nodeData(node).module_decl;

        var ports_list = std.ArrayList(ir.Port).empty;
        defer ports_list.deinit(self.gpa);
        var vars_list = std.ArrayList(ir.VarDecl).empty;
        defer vars_list.deinit(self.gpa);
        var processes = std.ArrayList(ir.Process).empty;
        defer processes.deinit(self.gpa);
        var stmts = std.ArrayList(ir.Stmt).empty;
        defer stmts.deinit(self.gpa);
        var exprs = std.ArrayList(ir.Expr).empty;
        defer exprs.deinit(self.gpa);

        for (self.ast.listSlice(data.ports)) |port_idx| {
            const port_node: Ast.Node.Index = @enumFromInt(port_idx);
            const port_data = self.ast.nodeData(port_node).port;
            const port_name = self.ast.tokenSlice(self.ast.nodeMainToken(port_node));
            const dir_tag: Token.Tag = @enumFromInt(port_data.dir);
            const dir: ir.Direction = switch (dir_tag) {
                .keyword_input => .input,
                .keyword_output => .output,
                .keyword_inout => .inout,
                else => .input,
            };
            const ty = try self.resolveType(port_data.ty) orelse try self.defaultType();
            try ports_list.append(self.gpa, .{ .name = port_name, .dir = dir, .ty = ty });
        }

        const body_data = self.ast.nodeData(data.body).block;
        for (self.ast.listSlice(body_data.statements)) |stmt_idx| {
            const stmt_node: Ast.Node.Index = @enumFromInt(stmt_idx);
            switch (self.ast.nodeTag(stmt_node)) {
                .var_decl => {
                    const var_data = self.ast.nodeData(stmt_node).var_decl;
                    const var_name = self.ast.tokenSlice(var_data.name);
                    const ty = try self.resolveType(var_data.ty) orelse try self.defaultType();
                    try vars_list.append(self.gpa, .{ .name = var_name, .ty = ty });
                },
                .always_ff => {
                    try processes.append(self.gpa, try self.lowerProcessAlways(&stmts, &exprs, stmt_node));
                },
                .comb => {
                    try processes.append(self.gpa, try self.lowerProcessComb(&stmts, &exprs, stmt_node));
                },
                else => {},
            }
        }

        return ir.Module{
            .name = name,
            .ports = try ports_list.toOwnedSlice(self.gpa),
            .vars = try vars_list.toOwnedSlice(self.gpa),
            .processes = try processes.toOwnedSlice(self.gpa),
            .stmts = try stmts.toOwnedSlice(self.gpa),
            .exprs = try exprs.toOwnedSlice(self.gpa),
        };
    }
};

pub fn lower(gpa: std.mem.Allocator, ast: *const Ast.Ast, analysis: *analyze.Analysis) !Lowered {
    var lowerer = Lowerer.init(gpa, ast, analysis);
    defer lowerer.deinit();

    var modules = std.ArrayList(ir.Module).empty;
    defer modules.deinit(gpa);

    const decls = ast.nodeData(ast.root).root.decls;
    for (ast.listSlice(decls)) |decl_idx| {
        const decl: Ast.Node.Index = @enumFromInt(decl_idx);
        const actual = if (ast.nodeTag(decl) == .@"comptime") ast.nodeData(decl).unary else decl;
        switch (ast.nodeTag(actual)) {
            .module_decl, .pub_module_decl => {
                try modules.append(gpa, try lowerer.lowerModule(actual));
            },
            else => {},
        }
    }

    const errors = try lowerer.errors.toOwnedSlice(gpa);
    errdefer gpa.free(errors);

    return Lowered{
        .ir = .{ .modules = try modules.toOwnedSlice(gpa) },
        .errors = errors,
    };
}

fn pushExpr(exprs: *std.ArrayList(ir.Expr), expr: ir.Expr, gpa: std.mem.Allocator) !ir.ExprId {
    const idx = exprs.items.len;
    try exprs.append(gpa, expr);
    return @intCast(idx);
}
