const std = @import("std");
const expressions = @import("expressions.zig");
const Expr = expressions.Expr;
const Token = @import("lexer.zig").Token;
const TokenType = @import("lexer.zig").TokenType;

const StringifyError = error{Error};

pub fn stringifyExpr(
    allocator: std.mem.Allocator,
    expr: *Expr,
) StringifyError![]u8 {
    return switch (expr.*) {
        .binary => stringifyBinary(allocator, expr.*.binary.*),
        .unary => stringifyUnary(allocator, expr.*.unary.*),
        .literal => stringifyLiteral(allocator, expr.*.literal.*),
        .grouping => stringifyGrouping(allocator, expr.*.grouping),
    };
}

fn stringifyUnary(
    allocator: std.mem.Allocator,
    expr: expressions.Unary,
) StringifyError![]u8 {
    return std.fmt.allocPrint(allocator, "({s} {s})", .{
        expr.operator.lexeme,
        stringifyExpr(allocator, expr.right) catch return StringifyError.Error,
    }) catch return StringifyError.Error;
}

fn stringifyBinary(
    allocator: std.mem.Allocator,
    expr: expressions.Binary,
) StringifyError![]u8 {
    return std.fmt.allocPrint(allocator, "({s} {s} {s})", .{
        expr.operator.lexeme,
        stringifyExpr(allocator, expr.left) catch return StringifyError.Error,
        stringifyExpr(allocator, expr.right) catch return StringifyError.Error,
    }) catch return StringifyError.Error;
}

fn stringifyLiteral(
    allocator: std.mem.Allocator,
    expr: expressions.Literal,
) StringifyError![]u8 {
    return switch (expr) {
        .BOOL => blk: {
            if (expr.BOOL) {
                var t = "true".*;
                break :blk &t;
            }
            var f = "false".*;
            break :blk &f;
        },
        .STRING => blk: {
            var str = "some string".*;
            break :blk &str;
        },
        .NUMBER => blk: {
            const str = std.fmt.allocPrint(
                allocator,
                "{d}",
                .{expr.NUMBER},
            ) catch return StringifyError.Error;
            break :blk str;
        },
        .NIL => blk: {
            var n = "nil".*;
            break :blk &n;
        },
    };
}

fn stringifyGrouping(
    allocator: std.mem.Allocator,
    expr: *Expr,
) StringifyError![]u8 {
    return std.fmt.allocPrint(
        allocator,
        "(group {s})",
        .{stringifyExpr(allocator, expr) catch return StringifyError.Error},
    ) catch return StringifyError.Error;
}
