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
    aliases: std.StringHashMap(TypeId),
    errors: []const Error,

    pub fn deinit(self: *Analysis, allocator: std.mem.Allocator) void {
        self.types.deinit(allocator);
        self.aliases.deinit();
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
            if (analysis.aliases.get(name)) |id| return id;
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
    alias_decl: std.StringHashMap(Ast.Node.Index),
    aliases: std.StringHashMap(TypeId),
    resolving: std.StringHashMap(void),
    errors: std.ArrayList(Error),

    fn init(gpa: std.mem.Allocator, ast: *const Ast.Ast) Analyzer {
        return .{
            .gpa = gpa,
            .ast = ast,
            .types = TypePool.init(),
            .alias_decl = std.StringHashMap(Ast.Node.Index).init(gpa),
            .aliases = std.StringHashMap(TypeId).init(gpa),
            .resolving = std.StringHashMap(void).init(gpa),
            .errors = std.ArrayList(Error).empty,
        };
    }

    fn deinit(self: *Analyzer) void {
        self.types.deinit(self.gpa);
        self.alias_decl.deinit();
        self.aliases.deinit();
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

    fn isTypeExprNode(self: *Analyzer, node: Ast.Node.Index) bool {
        const actual = self.unwrapComptime(node);
        return switch (self.ast.nodeTag(actual)) {
            .identifier, .builtin_ref, .call => true,
            else => false,
        };
    }

    fn collectAliases(self: *Analyzer, root: Ast.Node.Index) !void {
        const data = self.ast.nodeData(root).root;
        for (self.ast.listSlice(data.decls)) |decl_idx| {
            const decl: Ast.Node.Index = @enumFromInt(decl_idx);
            const actual = self.unwrapComptime(decl);
            if (self.ast.nodeTag(actual) != .const_decl) continue;
            const value = self.ast.nodeData(actual).const_decl.value;
            if (!self.isTypeExprNode(value)) continue;
            const name_tok = self.ast.nodeData(actual).const_decl.name;
            const name = self.ast.tokenSlice(name_tok);
            try self.alias_decl.put(name, actual);
        }
    }

    fn resolveAlias(self: *Analyzer, name: []const u8) std.mem.Allocator.Error!?TypeId {
        if (self.aliases.get(name)) |id| return id;
        if (self.resolving.contains(name)) {
            try self.addError(0, "cyclic type alias");
            return null;
        }
        const decl = self.alias_decl.get(name) orelse return null;
        try self.resolving.put(name, {});
        const value = self.ast.nodeData(decl).const_decl.value;
        const ty = try self.resolveTypeExpr(value);
        _ = self.resolving.remove(name);
        if (ty) |id| {
            try self.aliases.put(name, id);
        }
        return ty;
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
                if (try self.resolveAlias(name)) |id| return id;
                try self.addError(self.ast.nodeMainToken(node), "unknown type");
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
        if (self.ast.nodeTag(node) != .number_literal) {
            try self.addError(self.ast.nodeMainToken(node), "expected bit width");
            return null;
        }
        const text = self.ast.tokenSlice(self.ast.nodeMainToken(node));
        return std.fmt.parseUnsigned(u32, text, 10) catch {
            try self.addError(self.ast.nodeMainToken(node), "invalid bit width");
            return null;
        };
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
    defer analyzer.alias_decl.deinit();
    defer analyzer.resolving.deinit();

    const root = ast.root;
    try analyzer.collectAliases(root);

    const decls = ast.nodeData(root).root.decls;
    for (ast.listSlice(decls)) |decl_idx| {
        const decl: Ast.Node.Index = @enumFromInt(decl_idx);
        const actual = analyzer.unwrapComptime(decl);
        const tag = ast.nodeTag(actual);
        switch (tag) {
            .const_decl => {
                const value = ast.nodeData(actual).const_decl.value;
                if (!analyzer.isTypeExprNode(value)) continue;
                const name_tok = ast.nodeData(actual).const_decl.name;
                const name = ast.tokenSlice(name_tok);
                _ = try analyzer.resolveAlias(name);
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
    const aliases = analyzer.aliases;
    analyzer.aliases = std.StringHashMap(TypeId).init(allocator);

    return Analysis{
        .types = types,
        .aliases = aliases,
        .errors = errors,
    };
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
    const bits = try parseBits(allocator, ast, bits_node, errors) orelse return null;
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
    node: Ast.Node.Index,
    errors: *std.ArrayList(Error),
) !?u32 {
    if (ast.nodeTag(node) != .number_literal) {
        try errors.append(allocator, .{ .token = ast.nodeMainToken(node), .message = "expected bit width" });
        return null;
    }
    const text = ast.tokenSlice(ast.nodeMainToken(node));
    return std.fmt.parseUnsigned(u32, text, 10) catch {
        try errors.append(allocator, .{ .token = ast.nodeMainToken(node), .message = "invalid bit width" });
        return null;
    };
}
