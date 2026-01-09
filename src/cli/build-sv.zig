const std = @import("std");
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

    const file = res.positionals[0] orelse return error.MissingFile;

    std.debug.print("TODO: compile: {s}\n", .{file});
}

const params = clap.parseParamsComptime(
    \\-h, --help  Display this help and exit.
    \\<FILE>
);

const parsers = .{
    .FILE = clap.parsers.string,
};
