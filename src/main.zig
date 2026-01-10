const std = @import("std");
const clap = @import("clap");

const cli = @import("cli");

pub fn main() !void {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa_state.deinit();
    const gpa = gpa_state.allocator();

    var iter = try std.process.ArgIterator.initWithAllocator(gpa);
    defer iter.deinit();

    _ = iter.next();

    var diag = clap.Diagnostic{};
    var res = clap.parseEx(clap.Help, &params, parsers, &iter, .{
        .diagnostic = &diag,
        .allocator = gpa,
        .terminating_positional = 0,
    }) catch |err| {
        try diag.reportToFile(.stderr(), err);
        return err;
    };
    defer res.deinit();

    if (res.args.help != 0) {
        return clap.helpToFile(.stderr(), clap.Help, &params, .{});
    }

    const command = res.positionals[0] orelse return error.MissingCommand;
    switch (command) {
        .help => std.debug.print("--help\n", .{}),
        .build => {
            // TODO:
        },
        .init => {
            // TODO:
        },
        .@"build-sv" => {
            try cli.buildSv(gpa, &iter);
        },
        .@"test" => {
            // TODO:
        },
        .@"ast-check" => {
            try cli.astCheck(gpa, &iter);
        },
        .@"dump-ast" => {
            try cli.astCheck(gpa, &iter);
        },
        .fmt => {
            // TODO:
        },
    }
}

const SubCommands = enum {
    help,
    build,
    init,

    @"build-sv",
    @"test",
    @"ast-check",
    @"dump-ast",
    fmt,
};

const params = clap.parseParamsComptime(
    \\-h, --help  Display this help and exit.
    \\<command>
    \\
);

const parsers = .{
    .command = clap.parsers.enumeration(SubCommands),
};
