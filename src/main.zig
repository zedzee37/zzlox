const std = @import("std");

const lexer = @import("lexer.zig");
const lex = lexer.lex;
const LexerError = lexer.LexerError;
const LexerPayload = lexer.LexerPayload;
const Token = lexer.Token;

const parser = @import("parser.zig");
const parse = parser.parse;
const ParserPayload = parser.ParserPayload;
const ParserError = parser.ParserError;

const ast_printer = @import("ast_printer.zig");
const stringifyExpr = ast_printer.stringify;

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
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();

    var lexer_payload = LexerPayload.init();
    const tokens = lex(allocator, source, &lexer_payload) catch |err| {
        handleLexerError(err, lexer_payload);
        return err;
    };

    var parser_payload = ParserPayload.init();
    const expr = parse(&arena, tokens, &parser_payload) catch |err| {
        handleParserError(err, parser_payload);
        return err;
    };
    const str = stringifyExpr(&arena, expr) catch {
        std.debug.print("guh", .{});
        return;
    };

    std.debug.print("{s}\n", .{str.data});
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

fn handleParserError(err: ParserError, payload: ParserPayload) void {
    const stdErr = std.io.getStdErr().writer();
    switch (err) {
        ParserError.ExpectedToken => stdErr.print(
            "Unexpected token at line {d}, found: {s}, expected: {s}.\n",
            .{ payload.line.?, @tagName(payload.found.?), @tagName(payload.expected.?) },
        ) catch {},
        ParserError.CouldNotParse => stdErr.print(
            "An unexpected error occurred, could not parse.\n",
            .{},
        ) catch {},
        ParserError.UnexpectedToken => stdErr.print(
            "Reached an unexpected token.\n",
            .{},
        ) catch {},
        ParserError.CouldNotAllocate => stdErr.print(
            "Could not allocate room for expression.\n",
            .{},
        ) catch {},
        ParserError.OutOfBoundsError => stdErr.print(
            "Attempted to access index out of bounds, length {d}, index: {d}.\n",
            .{ payload.length.?, payload.idx.? },
        ) catch {},
    }
}
