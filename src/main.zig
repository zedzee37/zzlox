const std = @import("std");
const ast_printer = @import("ast_printer.zig");

const lexer = @import("lexer.zig");
const lex = lexer.Lexer.lex;
const Token = lexer.Token;
const TokenType = lexer.TokenType;

const expressions = @import("expressions.zig.");
const Expr = expressions.Expr;

const parser = @import("parser.zig");
const parse = parser.Parser.parse;

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

    try run(buffer);
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
        try run(buffer);
    }
}

fn run(source: []u8) !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();

    const tokens = lex(allocator, source).?;
    const expr = parse(&arena, tokens).?;
    const str = try ast_printer.stringifyExpr(allocator, expr);

    std.debug.print("{s}\n", .{str});
}
