const std = @import("std");

const lexer = @import("lexer.zig");
const lex = lexer.lex;
const LexerError = lexer.LexerError;
const LexerPayload = lexer.LexerPayload;
const Token = lexer.Token;

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    const file = getFileArg();

    if (file) |f| {
        try runFile(allocator, f);
    } else {
        try runPrompt(allocator);
    }
}

fn getFileArg() ?[]const u8 {
    const allocator = std.heap.page_allocator;
    var args = std.process.argsWithAllocator(allocator) catch {
        return null;
    };
    defer args.deinit();

    _ = args.next();
    return args.next();
}

fn runFile(allocator: std.mem.Allocator, file: []const u8) !void {
    const fp = try std.fs.cwd().openFile(file, .{});
    defer fp.close();

    const size = try fp.getEndPos();
    const buffer = try fp.readToEndAlloc(allocator, size);
    defer allocator.free(buffer);

    run(buffer) catch @panic("Found error, quitting.\n");
}

fn runPrompt(allocator: std.mem.Allocator) !void {
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    while (true) {
        try stdout.print("> ", .{});
        const buffer = try stdin.readUntilDelimiterAlloc(
            allocator,
            '\n',
            std.math.maxInt(usize),
        );
        defer allocator.free(buffer);
        run(buffer) catch {}; // we dont want to kill the prompt if it errors
    }
}

fn run(source: []u8) !void {
    const allocator = std.heap.page_allocator;

    var payload = LexerPayload.init();
    const tokens = lex(allocator, source, &payload) catch |err| {
        handleLexerError(err, payload);
        return err;
    };

    const std_out = std.io.getStdOut().writer();
    for (tokens) |tok| {
        const str = try tok.string(allocator);
        defer str.deinit();
        try std_out.print("{s}\n", .{str.data});
    }

    defer allocator.free(tokens);
}

fn handleLexerError(err: LexerError, payload: LexerPayload) void {
    const stdErr = std.io.getStdErr().writer();
    switch (err) {
        LexerError.UnexpectedCharacter => stdErr.print(
            "Unexpected character at {d}:{d}, found: {c}.\n",
            .{ payload.line.?, payload.where.?, payload.found.? },
        ) catch {},
        LexerError.OutOfRange => stdErr.print(
            "Tried to read character out of range.\n",
            .{},
        ) catch {},
        LexerError.CouldNotParseNumber => stdErr.print(
            "Could not parse number at: {d}.",
            .{payload.line.?},
        ) catch {},
        LexerError.ExpectedCharacter => stdErr.print(
            "Unexpected character at {d}:{d}, found: {c}, expected {c}.\n",
            .{
                payload.line.?,
                payload.where.?,
                payload.found.?,
                payload.expected.?,
            },
        ) catch {},
        else => {},
    }
}
