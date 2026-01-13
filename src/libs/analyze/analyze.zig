const std = @import("std");
const Ast = @import("../parse/Ast.zig");
const BuiltinFn = @import("../common/BuiltinFn.zig");
const primitives = @import("../common/primitives.zig");

pub const Signedness = enum { unsigned, signed };

pub const ClockKind = enum { posedge, negedge };
pub const ResetKind = enum { async_low, async_high, sync_low, sync_high };

pub const Type = union(enum) {
    clock: ClockKind,
    reset: ResetKind,
    bit: struct { signedness: Signedness, bits: u32 },
    logic: struct { signedness: Signedness, bits: u32 },
    anyclock,
    anyreset,
};

pub const TypeId = enum(u32) { _ };

pub const ComptimeValue = union(enum) {
    int: u64,
    @"type": TypeId,
};

pub const TypePool = struct {
    types: std.ArrayList(Type),

    pub fn init() TypePool {
        return .{ .types = std.ArrayList(Type).empty };
    }

    pub fn deinit(self: *TypePool, allocator: std.mem.Allocator) void {
        self.types.deinit(allocator);
    }

    pub fn get(self: *const TypePool, id: TypeId) Type {
        return self.types.items[@intFromEnum(id)];
    }

    pub fn getOrAdd(self: *TypePool, allocator: std.mem.Allocator, ty: Type) !TypeId {
        for (self.types.items, 0..) |existing, i| {
            if (typeEql(existing, ty)) {
                return @enumFromInt(@as(u32, @intCast(i)));
            }
        }
        try self.types.append(allocator, ty);
        return @enumFromInt(@as(u32, @intCast(self.types.items.len - 1)));
    }

    fn typeEql(a: Type, b: Type) bool {
        return switch (a) {
            .clock => |ak| switch (b) { .clock => |bk| ak == bk, else => false },
            .reset => |ak| switch (b) { .reset => |bk| ak == bk, else => false },
            .bit => |ai| switch (b) { .bit => |bi| ai.signedness == bi.signedness and ai.bits == bi.bits, else => false },
            .logic => |ai| switch (b) { .logic => |bi| ai.signedness == bi.signedness and ai.bits == bi.bits, else => false },
            .anyclock => switch (b) { .anyclock => true, else => false },
            .anyreset => switch (b) { .anyreset => true, else => false },
        };
    }
};

pub const Error = struct {
    token: Ast.TokenIndex,
    message: []const u8,
};

pub const Analysis = struct {
    types: TypePool,
    const_values: std.StringHashMap(ComptimeValue),
    errors: []const Error,

    pub fn deinit(self: *Analysis, allocator: std.mem.Allocator) void {
        self.types.deinit(allocator);
        self.const_values.deinit();
        allocator.free(self.errors);
        self.* = undefined;
    }
};

pub fn resolveTypeExpr(
    allocator: std.mem.Allocator,
    ast: *const Ast.Ast,
    analysis: *Analysis,
    node: Ast.Node.Index,
    errors: *std.ArrayList(Error),
) !?TypeId {
    const tag = ast.nodeTag(node);
    const data = ast.nodeData(node);
    switch (tag) {
        .identifier => {
            const name = ast.tokenSlice(ast.nodeMainToken(node));
            if (primitives.parseLogicBits(name)) |info| {
                const signedness: Signedness = switch (info.signedness) { .unsigned => .unsigned, .signed => .signed };
                const ty: Type = switch (info.kind) {
                    .logic => .{ .logic = .{ .signedness = signedness, .bits = info.bits } },
                    .bit => .{ .bit = .{ .signedness = signedness, .bits = info.bits } },
                };
                return @as(?TypeId, try analysis.types.getOrAdd(allocator, ty));
            }
            if (builtinType(name)) |ty| {
                return @as(?TypeId, try analysis.types.getOrAdd(allocator, ty));
            }
            if (analysis.const_values.get(name)) |value| {
                switch (value) {
                    .@"type" => |id| return id,
                    .int => {
                        try errors.append(allocator, .{ .token = ast.nodeMainToken(node), .message = "expected type" });
                        return null;
                    },
                }
            }
            try errors.append(allocator, .{ .token = ast.nodeMainToken(node), .message = "unknown type" });
            return null;
        },
        .call => {
            const callee = data.call.callee;
            if (ast.nodeTag(callee) != .builtin_ref) {
                try errors.append(allocator, .{ .token = ast.nodeMainToken(node), .message = "expected builtin type constructor" });
                return null;
            }
            const builtin_name = ast.tokenSlice(ast.nodeMainToken(callee));
            return resolveBuiltinCall(allocator, ast, analysis, ast.nodeMainToken(callee), builtin_name, data.call.args, errors);
        },
        .builtin_ref => {
            const name = ast.tokenSlice(ast.nodeMainToken(node));
            return resolveBuiltinCall(allocator, ast, analysis, ast.nodeMainToken(node), name, .{ .start = 0, .end = 0 }, errors);
        },
        .if_expr => {
            const cond = try evalComptimeBool(allocator, ast, analysis, data.if_expr.cond, errors, "expected comptime condition") orelse return null;
            const branch = if (cond) data.if_expr.then_expr else data.if_expr.else_expr;
            return resolveTypeExpr(allocator, ast, analysis, branch, errors);
        },
        .switch_expr => {
            const cond_value = try evalComptimeInt(allocator, ast, analysis, data.switch_expr.cond, errors, "expected comptime int") orelse return null;
            for (ast.listSlice(data.switch_expr.cases)) |case_idx| {
                const case_node: Ast.Node.Index = @enumFromInt(case_idx);
                const case_data = ast.nodeData(case_node).switch_case;
                const case_value = try evalComptimeInt(allocator, ast, analysis, case_data.value, errors, "expected comptime int") orelse return null;
                if (case_value == cond_value) {
                    return resolveTypeExpr(allocator, ast, analysis, case_data.expr, errors);
                }
            }
            if (data.switch_expr.else_expr.unwrap()) |else_expr| {
                return resolveTypeExpr(allocator, ast, analysis, else_expr, errors);
            }
            try errors.append(allocator, .{ .token = ast.nodeMainToken(node), .message = "switch is missing else" });
            return null;
        },
        else => {
            try errors.append(allocator, .{ .token = ast.nodeMainToken(node), .message = "expected type expression" });
            return null;
        },
    }
}

const Analyzer = struct {
    gpa: std.mem.Allocator,
    ast: *const Ast.Ast,
    types: TypePool,
    const_decl: std.StringHashMap(Ast.Node.Index),
    const_values: std.StringHashMap(ComptimeValue),
    resolving: std.StringHashMap(void),
    errors: std.ArrayList(Error),

    fn init(gpa: std.mem.Allocator, ast: *const Ast.Ast) Analyzer {
        return .{
            .gpa = gpa,
            .ast = ast,
            .types = TypePool.init(),
            .const_decl = std.StringHashMap(Ast.Node.Index).init(gpa),
            .const_values = std.StringHashMap(ComptimeValue).init(gpa),
            .resolving = std.StringHashMap(void).init(gpa),
            .errors = std.ArrayList(Error).empty,
        };
    }

    fn deinit(self: *Analyzer) void {
        self.types.deinit(self.gpa);
        self.const_decl.deinit();
        self.const_values.deinit();
        self.resolving.deinit();
        self.errors.deinit(self.gpa);
    }

    fn addError(self: *Analyzer, token: Ast.TokenIndex, message: []const u8) !void {
        try self.errors.append(self.gpa, .{ .token = token, .message = message });
    }

    fn unwrapComptime(self: *Analyzer, node: Ast.Node.Index) Ast.Node.Index {
        const data = self.ast.nodeData(node);
        return if (self.ast.nodeTag(node) == .@"comptime") data.unary else node;
    }

    fn collectConstDecls(self: *Analyzer, root: Ast.Node.Index) !void {
        const data = self.ast.nodeData(root).root;
        for (self.ast.listSlice(data.decls)) |decl_idx| {
            const decl: Ast.Node.Index = @enumFromInt(decl_idx);
            const actual = self.unwrapComptime(decl);
            if (self.ast.nodeTag(actual) != .const_decl) continue;
            const name_tok = self.ast.nodeData(actual).const_decl.name;
            const name = self.ast.tokenSlice(name_tok);
            try self.const_decl.put(name, actual);
        }
    }

    fn resolveConstValue(self: *Analyzer, name: []const u8) std.mem.Allocator.Error!?ComptimeValue {
        if (self.const_values.get(name)) |value| return value;
        if (self.resolving.contains(name)) {
            try self.addError(0, "cyclic comptime value");
            return null;
        }
        const decl = self.const_decl.get(name) orelse return null;
        try self.resolving.put(name, {});
        const value_node = self.ast.nodeData(decl).const_decl.value;
        const value = try self.evalComptimeValue(value_node);
        _ = self.resolving.remove(name);
        if (value) |v| {
            try self.const_values.put(name, v);
        }
        return value;
    }

    fn evalComptimeValue(self: *Analyzer, node: Ast.Node.Index) std.mem.Allocator.Error!?ComptimeValue {
        const actual = self.unwrapComptime(node);
        switch (self.ast.nodeTag(actual)) {
            .number_literal => {
                const text = self.ast.tokenSlice(self.ast.nodeMainToken(actual));
                const value = std.fmt.parseUnsigned(u64, text, 10) catch {
                    try self.addError(self.ast.nodeMainToken(actual), "invalid integer literal");
                    return null;
                };
                return .{ .int = value };
            },
            .identifier => {
                const name = self.ast.tokenSlice(self.ast.nodeMainToken(actual));
                if (try self.resolveConstValue(name)) |value| return value;
                const ty = try self.resolveTypeExpr(actual);
                if (ty) |id| return .{ .@"type" = id };
                try self.addError(self.ast.nodeMainToken(actual), "unknown comptime value");
                return null;
            },
            .if_expr => {
                const cond = try self.evalComptimeBool(self.ast.nodeData(actual).if_expr.cond, "expected comptime condition") orelse return null;
                const branch = if (cond) self.ast.nodeData(actual).if_expr.then_expr else self.ast.nodeData(actual).if_expr.else_expr;
                return self.evalComptimeValue(branch);
            },
            .switch_expr => {
                const cond_value = try self.evalComptimeInt(self.ast.nodeData(actual).switch_expr.cond, "expected comptime int") orelse return null;
                for (self.ast.listSlice(self.ast.nodeData(actual).switch_expr.cases)) |case_idx| {
                    const case_node: Ast.Node.Index = @enumFromInt(case_idx);
                    const case_data = self.ast.nodeData(case_node).switch_case;
                    const case_value = try self.evalComptimeInt(case_data.value, "expected comptime int") orelse return null;
                    if (case_value == cond_value) {
                        return self.evalComptimeValue(case_data.expr);
                    }
                }
                if (self.ast.nodeData(actual).switch_expr.else_expr.unwrap()) |else_expr| {
                    return self.evalComptimeValue(else_expr);
                }
                try self.addError(self.ast.nodeMainToken(actual), "switch is missing else");
                return null;
            },
            .binary => {
                const int_value = try self.evalComptimeInt(actual, "expected comptime int") orelse return null;
                return .{ .int = int_value };
            },
            .call, .builtin_ref => {
                const ty = try self.resolveTypeExpr(actual);
                if (ty) |id| return .{ .@"type" = id };
                return null;
            },
            else => {
                try self.addError(self.ast.nodeMainToken(actual), "unsupported comptime value");
                return null;
            },
        }
    }

    fn evalComptimeBool(self: *Analyzer, node: Ast.Node.Index, err_msg: []const u8) std.mem.Allocator.Error!?bool {
        const actual = self.unwrapComptime(node);
        if (self.ast.nodeTag(actual) == .binary) {
            const data = self.ast.nodeData(actual).binary;
            const op_tag = self.ast.tokenTag(self.ast.nodeMainToken(actual));
            if (op_tag == .equal_equal or op_tag == .bang_equal or op_tag == .angle_bracket_left or op_tag == .angle_bracket_left_equal or op_tag == .angle_bracket_right or op_tag == .angle_bracket_right_equal) {
                const lhs = try self.evalComptimeInt(data.lhs, err_msg) orelse return null;
                const rhs = try self.evalComptimeInt(data.rhs, err_msg) orelse return null;
                return switch (op_tag) {
                    .equal_equal => lhs == rhs,
                    .bang_equal => lhs != rhs,
                    .angle_bracket_left => lhs < rhs,
                    .angle_bracket_left_equal => lhs <= rhs,
                    .angle_bracket_right => lhs > rhs,
                    .angle_bracket_right_equal => lhs >= rhs,
                    else => unreachable,
                };
            }
        }
        const value = try self.evalComptimeInt(actual, err_msg) orelse return null;
        return value != 0;
    }

    fn evalComptimeInt(self: *Analyzer, node: Ast.Node.Index, err_msg: []const u8) std.mem.Allocator.Error!?u64 {
        const actual = self.unwrapComptime(node);
        switch (self.ast.nodeTag(actual)) {
            .number_literal => {
                const text = self.ast.tokenSlice(self.ast.nodeMainToken(actual));
                return std.fmt.parseUnsigned(u64, text, 10) catch {
                    try self.addError(self.ast.nodeMainToken(actual), "invalid integer literal");
                    return null;
                };
            },
            .identifier => {
                const name = self.ast.tokenSlice(self.ast.nodeMainToken(actual));
                if (try self.resolveConstValue(name)) |value| {
                    switch (value) {
                        .int => |int_value| return int_value,
                        .@"type" => {
                            try self.addError(self.ast.nodeMainToken(actual), err_msg);
                            return null;
                        },
                    }
                }
                try self.addError(self.ast.nodeMainToken(actual), err_msg);
                return null;
            },
            .binary => {
                const data = self.ast.nodeData(actual).binary;
                const lhs = try self.evalComptimeInt(data.lhs, err_msg) orelse return null;
                const rhs = try self.evalComptimeInt(data.rhs, err_msg) orelse return null;
                const op_tag = self.ast.tokenTag(self.ast.nodeMainToken(actual));
                return switch (op_tag) {
                    .plus => std.math.add(u64, lhs, rhs) catch {
                        try self.addError(self.ast.nodeMainToken(actual), "comptime int overflow");
                        return null;
                    },
                    .minus => std.math.sub(u64, lhs, rhs) catch {
                        try self.addError(self.ast.nodeMainToken(actual), "comptime int underflow");
                        return null;
                    },
                    .asterisk => std.math.mul(u64, lhs, rhs) catch {
                        try self.addError(self.ast.nodeMainToken(actual), "comptime int overflow");
                        return null;
                    },
                    .slash => {
                        if (rhs == 0) {
                            try self.addError(self.ast.nodeMainToken(actual), "comptime division by zero");
                            return null;
                        }
                        return lhs / rhs;
                    },
                    .percent => {
                        if (rhs == 0) {
                            try self.addError(self.ast.nodeMainToken(actual), "comptime division by zero");
                            return null;
                        }
                        return lhs % rhs;
                    },
                    else => {
                        try self.addError(self.ast.nodeMainToken(actual), "unsupported comptime operator");
                        return null;
                    },
                };
            },
            .if_expr => {
                const cond = try self.evalComptimeBool(self.ast.nodeData(actual).if_expr.cond, err_msg) orelse return null;
                const branch = if (cond) self.ast.nodeData(actual).if_expr.then_expr else self.ast.nodeData(actual).if_expr.else_expr;
                return self.evalComptimeInt(branch, err_msg);
            },
            .switch_expr => {
                const cond_value = try self.evalComptimeInt(self.ast.nodeData(actual).switch_expr.cond, err_msg) orelse return null;
                for (self.ast.listSlice(self.ast.nodeData(actual).switch_expr.cases)) |case_idx| {
                    const case_node: Ast.Node.Index = @enumFromInt(case_idx);
                    const case_data = self.ast.nodeData(case_node).switch_case;
                    const case_value = try self.evalComptimeInt(case_data.value, err_msg) orelse return null;
                    if (case_value == cond_value) {
                        return self.evalComptimeInt(case_data.expr, err_msg);
                    }
                }
                if (self.ast.nodeData(actual).switch_expr.else_expr.unwrap()) |else_expr| {
                    return self.evalComptimeInt(else_expr, err_msg);
                }
                try self.addError(self.ast.nodeMainToken(actual), "switch is missing else");
                return null;
            },
            else => {
                try self.addError(self.ast.nodeMainToken(actual), err_msg);
                return null;
            },
        }
    }

    fn resolveTypeExpr(self: *Analyzer, node: Ast.Node.Index) std.mem.Allocator.Error!?TypeId {
        const tag = self.ast.nodeTag(node);
        const data = self.ast.nodeData(node);
        switch (tag) {
            .identifier => {
                const name = self.ast.tokenSlice(self.ast.nodeMainToken(node));
                if (primitives.parseLogicBits(name)) |info| {
                    const signedness: Signedness = switch (info.signedness) { .unsigned => .unsigned, .signed => .signed };
                    const ty: Type = switch (info.kind) {
                        .logic => .{ .logic = .{ .signedness = signedness, .bits = info.bits } },
                        .bit => .{ .bit = .{ .signedness = signedness, .bits = info.bits } },
                    };
                    return @as(?TypeId, try self.types.getOrAdd(self.gpa, ty));
                }
                if (builtinType(name)) |ty| {
                return @as(?TypeId, try self.types.getOrAdd(self.gpa, ty));
                }
                if (try self.resolveConstValue(name)) |value| {
                    switch (value) {
                        .@"type" => |id| return id,
                        .int => {
                            try self.addError(self.ast.nodeMainToken(node), "expected type");
                            return null;
                        },
                    }
                }
                try self.addError(self.ast.nodeMainToken(node), "unknown type");
                return null;
            },
            .if_expr => {
                if (try self.evalComptimeValue(node)) |value| {
                    switch (value) {
                        .@"type" => |id| return id,
                        .int => {
                            try self.addError(self.ast.nodeMainToken(node), "expected type");
                            return null;
                        },
                    }
                }
                return null;
            },
            .switch_expr => {
                if (try self.evalComptimeValue(node)) |value| {
                    switch (value) {
                        .@"type" => |id| return id,
                        .int => {
                            try self.addError(self.ast.nodeMainToken(node), "expected type");
                            return null;
                        },
                    }
                }
                return null;
            },
            .call => {
                const callee = data.call.callee;
                if (self.ast.nodeTag(callee) != .builtin_ref) {
                    try self.addError(self.ast.nodeMainToken(node), "expected builtin type constructor");
                    return null;
                }
                const builtin_name = self.ast.tokenSlice(self.ast.nodeMainToken(callee));
                return self.resolveBuiltinCall(self.ast.nodeMainToken(callee), builtin_name, data.call.args);
            },
            .builtin_ref => {
                const name = self.ast.tokenSlice(self.ast.nodeMainToken(node));
                return self.resolveBuiltinCall(self.ast.nodeMainToken(node), name, .{ .start = 0, .end = 0 });
            },
            else => {
                try self.addError(self.ast.nodeMainToken(node), "expected type expression");
                return null;
            },
        }
    }

    fn resolveBuiltinCall(self: *Analyzer, token: Ast.TokenIndex, name: []const u8, args: Ast.Node.SubRange) !?TypeId {
        const builtin = BuiltinFn.get(name) orelse {
            try self.addError(token, "unknown builtin");
            return null;
        };
        if (builtin.kind != .type_ctor) {
            try self.addError(token, "expected builtin type constructor");
            return null;
        }
        if (builtin.param_count) |count| {
            const arg_count = self.ast.listSlice(args).len;
            if (arg_count != count) {
                try self.addError(token, "incorrect argument count");
                return null;
            }
        }
        return switch (builtin.tag) {
            .logic => self.resolveLogicBit(args, .logic),
            .bit => self.resolveLogicBit(args, .bit),
        };
    }

    fn resolveLogicBit(self: *Analyzer, args: Ast.Node.SubRange, kind: enum { logic, bit }) !?TypeId {
        const items = self.ast.listSlice(args);
        if (items.len != 2) {
            try self.addError(0, "expected two arguments");
            return null;
        }
        const signedness_node: Ast.Node.Index = @enumFromInt(items[0]);
        const bits_node: Ast.Node.Index = @enumFromInt(items[1]);

        const signedness = try self.parseSignedness(signedness_node) orelse return null;
        const bits = try self.parseBits(bits_node) orelse return null;
        const ty: Type = switch (kind) {
            .logic => .{ .logic = .{ .signedness = signedness, .bits = bits } },
            .bit => .{ .bit = .{ .signedness = signedness, .bits = bits } },
        };
        return @as(?TypeId, try self.types.getOrAdd(self.gpa, ty));
    }

    fn parseSignedness(self: *Analyzer, node: Ast.Node.Index) !?Signedness {
        if (self.ast.nodeTag(node) != .enum_literal) {
            try self.addError(self.ast.nodeMainToken(node), "expected signedness enum");
            return null;
        }
        const name = self.ast.tokenSlice(self.ast.nodeMainToken(node));
        if (std.mem.eql(u8, name, "unsigned")) return .unsigned;
        if (std.mem.eql(u8, name, "signed")) return .signed;
        try self.addError(self.ast.nodeMainToken(node), "unknown signedness");
        return null;
    }

    fn parseBits(self: *Analyzer, node: Ast.Node.Index) !?u32 {
        const value = try self.evalComptimeInt(node, "expected bit width") orelse return null;
        return clampBits(self, self.ast.nodeMainToken(self.unwrapComptime(node)), value);
    }

    fn analyzeModule(self: *Analyzer, node: Ast.Node.Index) !void {
        const data = self.ast.nodeData(node).module_decl;
        for (self.ast.listSlice(data.ports)) |port_idx| {
            const port_node: Ast.Node.Index = @enumFromInt(port_idx);
            const port_data = self.ast.nodeData(port_node).port;
            _ = try self.resolveTypeExpr(port_data.ty);
        }

        const body_node = data.body;
        const body_data = self.ast.nodeData(body_node).block;
        for (self.ast.listSlice(body_data.statements)) |stmt_idx| {
            const stmt: Ast.Node.Index = @enumFromInt(stmt_idx);
            if (self.ast.nodeTag(stmt) == .var_decl) {
                const var_data = self.ast.nodeData(stmt).var_decl;
                _ = try self.resolveTypeExpr(var_data.ty);
            }
        }
    }
};

pub fn analyze(allocator: std.mem.Allocator, ast: *const Ast.Ast) !Analysis {
    var analyzer = Analyzer.init(allocator, ast);
    defer analyzer.const_decl.deinit();
    defer analyzer.resolving.deinit();

    const root = ast.root;
    try analyzer.collectConstDecls(root);

    const decls = ast.nodeData(root).root.decls;
    for (ast.listSlice(decls)) |decl_idx| {
        const decl: Ast.Node.Index = @enumFromInt(decl_idx);
        const actual = analyzer.unwrapComptime(decl);
        const tag = ast.nodeTag(actual);
        switch (tag) {
            .const_decl => {
                const name_tok = ast.nodeData(actual).const_decl.name;
                const name = ast.tokenSlice(name_tok);
                _ = try analyzer.resolveConstValue(name);
            },
            .module_decl, .pub_module_decl => {
                try analyzer.analyzeModule(actual);
            },
            else => {},
        }
    }

    const errors = try analyzer.errors.toOwnedSlice(allocator);
    errdefer allocator.free(errors);

    const types = analyzer.types;
    analyzer.types = TypePool.init();
    const const_values = analyzer.const_values;
    analyzer.const_values = std.StringHashMap(ComptimeValue).init(allocator);

    return Analysis{
        .types = types,
        .const_values = const_values,
        .errors = errors,
    };
}

fn clampBits(self: *Analyzer, token: Ast.TokenIndex, value: u64) !?u32 {
    if (value == 0 or value > std.math.maxInt(u32)) {
        try self.addError(token, "invalid bit width");
        return null;
    }
    return @intCast(value);
}

fn builtinType(name: []const u8) ?Type {
    if (std.mem.eql(u8, name, "clock_posedge")) return .{ .clock = .posedge };
    if (std.mem.eql(u8, name, "clock_negedge")) return .{ .clock = .negedge };
    if (std.mem.eql(u8, name, "reset_async_low")) return .{ .reset = .async_low };
    if (std.mem.eql(u8, name, "reset_async_high")) return .{ .reset = .async_high };
    if (std.mem.eql(u8, name, "reset_sync_low")) return .{ .reset = .sync_low };
    if (std.mem.eql(u8, name, "reset_sync_high")) return .{ .reset = .sync_high };
    if (std.mem.eql(u8, name, "anyclock")) return .anyclock;
    if (std.mem.eql(u8, name, "anyreset")) return .anyreset;
    return null;
}


fn resolveBuiltinCall(
    allocator: std.mem.Allocator,
    ast: *const Ast.Ast,
    analysis: *Analysis,
    token: Ast.TokenIndex,
    name: []const u8,
    args: Ast.Node.SubRange,
    errors: *std.ArrayList(Error),
) !?TypeId {
    const builtin = BuiltinFn.get(name) orelse {
        try errors.append(allocator, .{ .token = token, .message = "unknown builtin" });
        return null;
    };
    if (builtin.kind != .type_ctor) {
        try errors.append(allocator, .{ .token = token, .message = "expected builtin type constructor" });
        return null;
    }
    if (builtin.param_count) |count| {
        const arg_count = ast.listSlice(args).len;
        if (arg_count != count) {
            try errors.append(allocator, .{ .token = token, .message = "incorrect argument count" });
            return null;
        }
    }
    return switch (builtin.tag) {
        .logic => resolveLogicBit(allocator, ast, analysis, args, .logic, errors),
        .bit => resolveLogicBit(allocator, ast, analysis, args, .bit, errors),
    };
}

fn resolveLogicBit(
    allocator: std.mem.Allocator,
    ast: *const Ast.Ast,
    analysis: *Analysis,
    args: Ast.Node.SubRange,
    kind: enum { logic, bit },
    errors: *std.ArrayList(Error),
) !?TypeId {
    const items = ast.listSlice(args);
    if (items.len != 2) {
        try errors.append(allocator, .{ .token = 0, .message = "expected two arguments" });
        return null;
    }
    const signedness_node: Ast.Node.Index = @enumFromInt(items[0]);
    const bits_node: Ast.Node.Index = @enumFromInt(items[1]);

    const signedness = try parseSignedness(allocator, ast, signedness_node, errors) orelse return null;
    const bits = try parseBits(allocator, ast, analysis, bits_node, errors) orelse return null;
    const ty: Type = switch (kind) {
        .logic => .{ .logic = .{ .signedness = signedness, .bits = bits } },
        .bit => .{ .bit = .{ .signedness = signedness, .bits = bits } },
    };
    return @as(?TypeId, try analysis.types.getOrAdd(allocator, ty));
}

fn parseSignedness(
    allocator: std.mem.Allocator,
    ast: *const Ast.Ast,
    node: Ast.Node.Index,
    errors: *std.ArrayList(Error),
) !?Signedness {
    if (ast.nodeTag(node) != .enum_literal) {
        try errors.append(allocator, .{ .token = ast.nodeMainToken(node), .message = "expected signedness enum" });
        return null;
    }
    const name = ast.tokenSlice(ast.nodeMainToken(node));
    if (std.mem.eql(u8, name, "unsigned")) return .unsigned;
    if (std.mem.eql(u8, name, "signed")) return .signed;
    try errors.append(allocator, .{ .token = ast.nodeMainToken(node), .message = "unknown signedness" });
    return null;
}

fn parseBits(
    allocator: std.mem.Allocator,
    ast: *const Ast.Ast,
    analysis: *Analysis,
    node: Ast.Node.Index,
    errors: *std.ArrayList(Error),
) !?u32 {
    const value = try evalComptimeInt(allocator, ast, analysis, node, errors, "expected bit width") orelse return null;
    if (value == 0 or value > std.math.maxInt(u32)) {
        const token = ast.nodeMainToken(if (ast.nodeTag(node) == .@"comptime") ast.nodeData(node).unary else node);
        try errors.append(allocator, .{ .token = token, .message = "invalid bit width" });
        return null;
    }
    return @intCast(value);
}

fn evalComptimeBool(
    allocator: std.mem.Allocator,
    ast: *const Ast.Ast,
    analysis: *Analysis,
    node: Ast.Node.Index,
    errors: *std.ArrayList(Error),
    err_msg: []const u8,
) std.mem.Allocator.Error!?bool {
    const actual = if (ast.nodeTag(node) == .@"comptime") ast.nodeData(node).unary else node;
    if (ast.nodeTag(actual) == .binary) {
        const data = ast.nodeData(actual).binary;
        const op_tag = ast.tokenTag(ast.nodeMainToken(actual));
        if (op_tag == .equal_equal or op_tag == .bang_equal or op_tag == .angle_bracket_left or op_tag == .angle_bracket_left_equal or op_tag == .angle_bracket_right or op_tag == .angle_bracket_right_equal) {
            const lhs = try evalComptimeInt(allocator, ast, analysis, data.lhs, errors, err_msg) orelse return null;
            const rhs = try evalComptimeInt(allocator, ast, analysis, data.rhs, errors, err_msg) orelse return null;
            return switch (op_tag) {
                .equal_equal => lhs == rhs,
                .bang_equal => lhs != rhs,
                .angle_bracket_left => lhs < rhs,
                .angle_bracket_left_equal => lhs <= rhs,
                .angle_bracket_right => lhs > rhs,
                .angle_bracket_right_equal => lhs >= rhs,
                else => unreachable,
            };
        }
    }
    const value = try evalComptimeInt(allocator, ast, analysis, actual, errors, err_msg) orelse return null;
    return value != 0;
}

fn evalComptimeInt(
    allocator: std.mem.Allocator,
    ast: *const Ast.Ast,
    analysis: *Analysis,
    node: Ast.Node.Index,
    errors: *std.ArrayList(Error),
    err_msg: []const u8,
) std.mem.Allocator.Error!?u64 {
    const actual = if (ast.nodeTag(node) == .@"comptime") ast.nodeData(node).unary else node;
    switch (ast.nodeTag(actual)) {
        .number_literal => {
            const text = ast.tokenSlice(ast.nodeMainToken(actual));
            return std.fmt.parseUnsigned(u64, text, 10) catch {
                try errors.append(allocator, .{ .token = ast.nodeMainToken(actual), .message = "invalid integer literal" });
                return null;
            };
        },
        .identifier => {
            const name = ast.tokenSlice(ast.nodeMainToken(actual));
            if (analysis.const_values.get(name)) |value| {
                switch (value) {
                    .int => |int_value| return int_value,
                    .@"type" => {
                        try errors.append(allocator, .{ .token = ast.nodeMainToken(actual), .message = err_msg });
                        return null;
                    },
                }
            }
            try errors.append(allocator, .{ .token = ast.nodeMainToken(actual), .message = err_msg });
            return null;
        },
        .binary => {
            const data = ast.nodeData(actual).binary;
            const lhs = try evalComptimeInt(allocator, ast, analysis, data.lhs, errors, err_msg) orelse return null;
            const rhs = try evalComptimeInt(allocator, ast, analysis, data.rhs, errors, err_msg) orelse return null;
            const op_tag = ast.tokenTag(ast.nodeMainToken(actual));
            return switch (op_tag) {
                .plus => std.math.add(u64, lhs, rhs) catch {
                    try errors.append(allocator, .{ .token = ast.nodeMainToken(actual), .message = "comptime int overflow" });
                    return null;
                },
                .minus => std.math.sub(u64, lhs, rhs) catch {
                    try errors.append(allocator, .{ .token = ast.nodeMainToken(actual), .message = "comptime int underflow" });
                    return null;
                },
                .asterisk => std.math.mul(u64, lhs, rhs) catch {
                    try errors.append(allocator, .{ .token = ast.nodeMainToken(actual), .message = "comptime int overflow" });
                    return null;
                },
                .slash => {
                    if (rhs == 0) {
                        try errors.append(allocator, .{ .token = ast.nodeMainToken(actual), .message = "comptime division by zero" });
                        return null;
                    }
                    return lhs / rhs;
                },
                .percent => {
                    if (rhs == 0) {
                        try errors.append(allocator, .{ .token = ast.nodeMainToken(actual), .message = "comptime division by zero" });
                        return null;
                    }
                    return lhs % rhs;
                },
                else => {
                    try errors.append(allocator, .{ .token = ast.nodeMainToken(actual), .message = "unsupported comptime operator" });
                    return null;
                },
            };
        },
        .if_expr => {
            const cond = try evalComptimeBool(allocator, ast, analysis, ast.nodeData(actual).if_expr.cond, errors, err_msg) orelse return null;
            const branch = if (cond) ast.nodeData(actual).if_expr.then_expr else ast.nodeData(actual).if_expr.else_expr;
            return evalComptimeInt(allocator, ast, analysis, branch, errors, err_msg);
        },
        .switch_expr => {
            const cond_value = try evalComptimeInt(allocator, ast, analysis, ast.nodeData(actual).switch_expr.cond, errors, err_msg) orelse return null;
            for (ast.listSlice(ast.nodeData(actual).switch_expr.cases)) |case_idx| {
                const case_node: Ast.Node.Index = @enumFromInt(case_idx);
                const case_data = ast.nodeData(case_node).switch_case;
                const case_value = try evalComptimeInt(allocator, ast, analysis, case_data.value, errors, err_msg) orelse return null;
                if (case_value == cond_value) {
                    return evalComptimeInt(allocator, ast, analysis, case_data.expr, errors, err_msg);
                }
            }
            if (ast.nodeData(actual).switch_expr.else_expr.unwrap()) |else_expr| {
                return evalComptimeInt(allocator, ast, analysis, else_expr, errors, err_msg);
            }
            try errors.append(allocator, .{ .token = ast.nodeMainToken(actual), .message = "switch is missing else" });
            return null;
        },
        else => {
            try errors.append(allocator, .{ .token = ast.nodeMainToken(actual), .message = err_msg });
            return null;
        },
    }
}
