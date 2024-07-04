const std = @import("std");
const expressions = @import("expressions.zig");
const Expr = expressions.Expr;
const Token = @import("lexer.zig").Token;
const TokenType = @import("lexer.zig").TokenType;

const StringifyError = error{Error};

pub fn stringifyExpr(
    allocator: std.mem.Allocator,
    expr: Expr,
) StringifyError![]const u8 {
    return switch (expr) {
        .unary => stringifyUnary(allocator, expr.unary),
        .binary => stringifyBinary(allocator, expr.binary),
        .literal => stringifyLiteral(allocator, expr.literal),
        .grouping => stringifyGrouping(allocator, expr.grouping),
    };
}

fn stringifyUnary(
    allocator: std.mem.Allocator,
    expr: expressions.Unary,
) StringifyError![]const u8 {
    return std.fmt.allocPrint(allocator, "({s} {s})", .{
        expr.operator.lexeme,
        stringifyExpr(allocator, expr.right.*) catch return StringifyError.Error,
    }) catch return StringifyError.Error;
}

fn stringifyBinary(
    allocator: std.mem.Allocator,
    expr: expressions.Binary,
) StringifyError![]const u8 {
    return std.fmt.allocPrint(allocator, "({s} {s} {s})", .{
        expr.operator.lexeme,
        stringifyExpr(allocator, expr.left.*) catch return StringifyError.Error,
        stringifyExpr(allocator, expr.right.*) catch return StringifyError.Error,
    }) catch return StringifyError.Error;
}

fn stringifyLiteral(
    allocator: std.mem.Allocator,
    expr: expressions.Literal,
) StringifyError![]const u8 {
    return switch (expr) {
        .BOOL => blk: {
            if (expr.BOOL) {
                break :blk "true";
            }
            break :blk "false";
        },
        .STRING => expr.STRING,
        .NUMBER => blk: {
            const str = std.fmt.allocPrint(
                allocator,
                "{d}",
                .{expr.NUMBER},
            ) catch return StringifyError.Error;
            break :blk str;
        },
        .NIL => "nil",
    };
}

fn stringifyGrouping(
    allocator: std.mem.Allocator,
    expr: *const Expr,
) StringifyError![]const u8 {
    return std.fmt.allocPrint(
        allocator,
        "(group {s})",
        .{stringifyExpr(allocator, expr.*) catch return StringifyError.Error},
    ) catch return StringifyError.Error;
}

test "printing" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit(); // secret no memory leak trick no clickbait
    const allocator = arena.allocator();

    const literal1 = Expr.init(Expr.Tag.literal, expressions.Literal.init(expressions.Literal.Tag.NUMBER, 123));

    const literal2 = Expr.init(Expr.Tag.literal, expressions.Literal.init(expressions.Literal.Tag.NUMBER, 45.68));

    const unaryExpr = Expr.init(Expr.Tag.unary, expressions.Unary.init(Token.init(
        TokenType.init(TokenType.Tag.MINUS, {}),
        "-",
        1,
    ), &literal1));

    const groupingExpr = Expr.init(Expr.Tag.grouping, &literal2);

    const expr = Expr.init(
        Expr.Tag.binary,
        expressions.Binary.init(
            &unaryExpr,
            &groupingExpr,
            Token.init(TokenType.init(TokenType.Tag.STAR, {}), "*", 1),
        ),
    );

    const str = try stringifyExpr(allocator, expr);
    std.debug.print("{s}", .{str});
}
