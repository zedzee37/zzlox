const std = @import("std");
const Allocator = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;

const lexer = @import("lexer.zig");
const Token = lexer.Token;
const TokenTag = lexer.TokenTag;
const TokenType = lexer.TokenType;

const expressions = @import("expressions.zig");
const LiteralTag = expressions.LiteralTag;
const Literal = expressions.Literal;
const Unary = expressions.Unary;
const Binary = expressions.Binary;
const ExprTag = expressions.ExprTag;
const Expr = expressions.Expr;

const String = @import("string.zig").String;

const StringifyError = error{CouldNotAlloc};

pub fn stringify(arena: *ArenaAllocator, expr: *Expr) !String {
    return stringifyExpr(arena.allocator(), expr);
}

fn stringifyExpr(allocator: Allocator, expr: *Expr) StringifyError!String {
    return switch (expr.*) {
        .binary => stringifyBinary(allocator, expr.binary),
        .unary => stringifyUnary(allocator, expr.unary),
        .grouping => stringifyGrouping(allocator, expr.grouping),
        .literal => stringifyLiteral(allocator, expr.literal),
    };
}

fn stringifyBinary(allocator: Allocator, expr: *Binary) StringifyError!String {
    const leftStr = try stringifyExpr(allocator, expr.left);
    const rightStr = try stringifyExpr(allocator, expr.right);

    return String.fromFormat(
        allocator,
        "({s} {s} {s})",
        .{
            expr.operator.lexeme,
            leftStr.data,
            rightStr.data,
        },
    ) catch StringifyError.CouldNotAlloc;
}

fn stringifyUnary(allocator: Allocator, expr: *Unary) StringifyError!String {
    const exprStr = try stringifyExpr(allocator, expr.right);

    return String.fromFormat(allocator, "({s} {s})", .{
        expr.operator.lexeme,
        exprStr.data,
    }) catch return StringifyError.CouldNotAlloc;
}

fn stringifyGrouping(allocator: Allocator, expr: *Expr) StringifyError!String {
    const exprStr = try stringifyExpr(allocator, expr);

    return String.fromFormat(allocator, "(group {s})", .{
        exprStr.data,
    }) catch StringifyError.CouldNotAlloc;
}

fn stringifyLiteral(allocator: Allocator, expr: *Literal) StringifyError!String {
    return switch (expr.*) {
        .BOOL => if (expr.BOOL)
            return String.from(allocator, "true") catch return StringifyError.CouldNotAlloc
        else
            return String.from(allocator, "false") catch return StringifyError.CouldNotAlloc,
        .STRING => String.from(allocator, expr.STRING) catch return StringifyError.CouldNotAlloc,
        .NUMBER => String.fromFormat(allocator, "{d}", .{expr.NUMBER}) catch return StringifyError.CouldNotAlloc,
        .NIL => String.from(allocator, "nil") catch return StringifyError.CouldNotAlloc,
    };
}

test "stringify" {
    var arena = ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    var lit1 = Literal.init(LiteralTag.NUMBER, 3132.5151);
    var lit2 = Literal.init(LiteralTag.NUMBER, 3132.5151);

    var literal1 = Expr.init(ExprTag.literal, &lit1);
    var literal2 = Expr.init(ExprTag.literal, &lit2);

    var un = Unary.init(Token.init(
        TokenType.initEmpty(TokenTag.MINUS),
        0,
        "-",
    ), &literal1);

    var unary = Expr.init(ExprTag.unary, &un);
    var grouping = Expr.init(ExprTag.grouping, &literal2);

    var bin =
        Binary.init(
        &unary,
        &grouping,
        Token.init(
            TokenType.initEmpty(TokenTag.STAR),
            0,
            "*",
        ),
    );
    var binary = Expr.init(ExprTag.binary, &bin);

    const str = try stringify(&arena, &binary);
    std.debug.print("{s}\n", .{str.data});
}
