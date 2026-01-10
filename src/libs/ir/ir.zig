const std = @import("std");
const analyze = @import("../analyze/analyze.zig");

pub const TypeId = analyze.TypeId;

pub const Direction = enum { input, output, inout };

pub const AssignOp = enum { assign, add_assign };

pub const BinaryOp = enum { add, equal_equal };

pub const Expr = struct {
    tag: Tag,
    data: Data,

    pub const Tag = enum { ident, number, binary };

    pub const Data = union(enum) {
        ident: []const u8,
        number: u64,
        binary: struct { op: BinaryOp, lhs: ExprId, rhs: ExprId },
    };
};

pub const ExprId = u32;

pub const Stmt = struct {
    tag: Tag,
    data: Data,

    pub const Tag = enum { assign, if_stmt };

    pub const Data = union(enum) {
        assign: struct { target: []const u8, op: AssignOp, value: ExprId },
        if_stmt: struct { cond: ExprId, then_body: Range, else_body: Range },
    };
};

pub const StmtId = u32;

pub const Range = struct { start: u32, end: u32 };

pub const Process = union(enum) {
    always_ff: struct { reset_body: Range, body: Range },
    comb: struct { body: Range },
};

pub const Port = struct {
    name: []const u8,
    dir: Direction,
    ty: TypeId,
};

pub const VarDecl = struct {
    name: []const u8,
    ty: TypeId,
};

pub const Module = struct {
    name: []const u8,
    ports: []Port,
    vars: []VarDecl,
    processes: []Process,
    stmts: []Stmt,
    exprs: []Expr,
};

pub const Ir = struct {
    modules: []Module,

    pub fn deinit(self: *Ir, allocator: std.mem.Allocator) void {
        for (self.modules) |*m| {
            allocator.free(m.ports);
            allocator.free(m.vars);
            allocator.free(m.processes);
            allocator.free(m.stmts);
            allocator.free(m.exprs);
        }
        allocator.free(self.modules);
        self.* = undefined;
    }
};
