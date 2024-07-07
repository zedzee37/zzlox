const std = @import("std");

const expressions = @import("expressions.zig");
const Expr = expressions.Expr;
const Literal = expressions.Literal;

const Token = @import("lexer.zig").Token;
const TokenType = @import("lexer.zig").TokenType;

pub const InterpreterError = error{
    ExpectedNumber,
    ExpectedBool,
    Unknown,
};

pub fn interpret(
    allocator: std.mem.Allocator,
    expr: *Expr,
) void {
    const lit = interpretExpr(allocator, expr) catch |err| {
        handleInterpreterError(err);
        return;
    };
    printLiteral(lit) catch {};
}

fn printLiteral(literal: Literal) !void {
    const out = std.io.getStdOut().writer();
    return switch (literal) {
        .NUMBER => out.print("{d}\n", .{literal.NUMBER}),
        .BOOL => blk: {
            if (!literal.BOOL) {
                break :blk out.print("false\n", .{});
            } else {
                break :blk out.print("true\n", .{});
            }
        },
        .STRING => out.print("{s}\n", .{literal.STRING}),
        .NIL => out.print("nil\n", .{}),
    };
}

fn interpretExpr(
    allocator: std.mem.Allocator,
    expr: *Expr,
) InterpreterError!Literal {
    return switch (expr.*) {
        .literal => interpretLiteral(expr.literal),
        .grouping => interpretGrouping(allocator, expr.grouping),
        .unary => interpretUnary(allocator, expr.unary),
        .binary => interpretBinary(allocator, expr.binary),
    };
}

fn interpretLiteral(literal: *Literal) !Literal {
    return literal.*;
}

fn interpretGrouping(allocator: std.mem.Allocator, grouping: *Expr) !Literal {
    return interpretExpr(allocator, grouping);
}

fn interpretUnary(
    allocator: std.mem.Allocator,
    unary: *expressions.Unary,
) !Literal {
    const right = try interpretExpr(allocator, unary.right);

    return switch (unary.operator.token_type) {
        .MINUS => blk: {
            if (@as(Literal.Tag, right) == Literal.Tag.NUMBER) {
                break :blk Literal.init(Literal.Tag.NUMBER, -right.NUMBER);
            }
            break :blk InterpreterError.ExpectedNumber;
        },
        .BANG => blk: {
            // i hate truthiness, i do not allow it.
            if (@as(Literal.Tag, right) == Literal.Tag.BOOL) {
                break :blk Literal.init(Literal.Tag.BOOL, !right.BOOL);
            }
            break :blk InterpreterError.ExpectedBool;
        },
        else => InterpreterError.Unknown,
    };
}

fn interpretBinary(
    allocator: std.mem.Allocator,
    binary: *expressions.Binary,
) !Literal {
    const left = try interpretExpr(allocator, binary.left);
    const right = try interpretExpr(allocator, binary.right);

    return switch (binary.operator.token_type) {
        .MINUS => blk: {
            if (@as(Literal.Tag, right) == Literal.Tag.NUMBER and
                @as(Literal.Tag, left) == Literal.Tag.NUMBER)
            {
                break :blk Literal.init(
                    Literal.Tag.NUMBER,
                    left.NUMBER - right.NUMBER,
                );
            }
            break :blk InterpreterError.ExpectedNumber;
        },
        .SLASH => blk: {
            if (@as(Literal.Tag, right) == Literal.Tag.NUMBER and
                @as(Literal.Tag, left) == Literal.Tag.NUMBER)
            {
                break :blk Literal.init(
                    Literal.Tag.NUMBER,
                    left.NUMBER / right.NUMBER,
                );
            }
            break :blk InterpreterError.ExpectedNumber;
        },
        .STAR => blk: {
            if (@as(Literal.Tag, right) == Literal.Tag.NUMBER and
                @as(Literal.Tag, left) == Literal.Tag.NUMBER)
            {
                break :blk Literal.init(
                    Literal.Tag.NUMBER,
                    left.NUMBER * right.NUMBER,
                );
            }
            break :blk InterpreterError.ExpectedNumber;
        },
        .PLUS => blk: {
            if (@as(Literal.Tag, right) == Literal.Tag.NUMBER and
                @as(Literal.Tag, left) == Literal.Tag.NUMBER)
            {
                break :blk Literal.init(
                    Literal.Tag.NUMBER,
                    left.NUMBER + right.NUMBER,
                );
            } else if (@as(Literal.Tag, right) == Literal.Tag.STRING and
                @as(Literal.Tag, left) == Literal.Tag.STRING)
            {
                const str =
                    std.fmt.allocPrint(allocator, "{s}{s}", .{
                    left.STRING,
                    right.STRING,
                }) catch break :blk InterpreterError.Unknown;
                break :blk Literal.init(Literal.Tag.STRING, str);
            }

            break :blk InterpreterError.Unknown;
        },
        .GREATER => blk: {
            if (@as(Literal.Tag, right) == Literal.Tag.NUMBER and
                @as(Literal.Tag, left) == Literal.Tag.NUMBER)
            {
                break :blk Literal.init(
                    Literal.Tag.BOOL,
                    left.NUMBER > right.NUMBER,
                );
            }
            break :blk InterpreterError.ExpectedNumber;
        },
        .GREATER_EQUAL => blk: {
            if (@as(Literal.Tag, right) == Literal.Tag.NUMBER and
                @as(Literal.Tag, left) == Literal.Tag.NUMBER)
            {
                break :blk Literal.init(
                    Literal.Tag.BOOL,
                    left.NUMBER >= right.NUMBER,
                );
            }
            break :blk InterpreterError.ExpectedNumber;
        },
        .LESS => blk: {
            if (@as(Literal.Tag, right) == Literal.Tag.NUMBER and
                @as(Literal.Tag, left) == Literal.Tag.NUMBER)
            {
                break :blk Literal.init(
                    Literal.Tag.BOOL,
                    left.NUMBER < right.NUMBER,
                );
            }
            break :blk InterpreterError.ExpectedNumber;
        },
        .LESS_EQUAL => blk: {
            if (@as(Literal.Tag, right) == Literal.Tag.NUMBER and
                @as(Literal.Tag, left) == Literal.Tag.NUMBER)
            {
                break :blk Literal.init(
                    Literal.Tag.BOOL,
                    left.NUMBER <= right.NUMBER,
                );
            }
            break :blk InterpreterError.ExpectedNumber;
        },
        .BANG_EQUAL => blk: {
            if (@as(Literal.Tag, right) == Literal.Tag.NUMBER and
                @as(Literal.Tag, left) == Literal.Tag.NUMBER)
            {
                break :blk Literal.init(
                    Literal.Tag.BOOL,
                    right.NUMBER != left.NUMBER,
                );
            } else if (@as(Literal.Tag, right) == Literal.Tag.STRING and
                @as(Literal.Tag, left) != Literal.Tag.STRING)
            {
                break :blk Literal.init(Literal.Tag.BOOL, std.mem.eql(
                    u8,
                    left.STRING,
                    right.STRING,
                ));
            } else if (@as(Literal.Tag, right) == Literal.Tag.BOOL and
                @as(Literal.Tag, left) == Literal.Tag.BOOL)
            {
                break :blk Literal.init(
                    Literal.Tag.BOOL,
                    right.BOOL != left.BOOL,
                );
            }

            break :blk Literal.init(Literal.Tag.BOOL, false);
        },
        .EQUAL_EQUAL => blk: {
            if (@as(Literal.Tag, right) == Literal.Tag.NUMBER and
                @as(Literal.Tag, left) == Literal.Tag.NUMBER)
            {
                break :blk Literal.init(
                    Literal.Tag.BOOL,
                    right.NUMBER == left.NUMBER,
                );
            } else if (@as(Literal.Tag, right) == Literal.Tag.STRING and
                @as(Literal.Tag, left) == Literal.Tag.STRING)
            {
                break :blk Literal.init(Literal.Tag.BOOL, std.mem.eql(
                    u8,
                    left.STRING,
                    right.STRING,
                ));
            } else if (@as(Literal.Tag, right) == Literal.Tag.BOOL and
                @as(Literal.Tag, left) == Literal.Tag.BOOL)
            {
                break :blk Literal.init(
                    Literal.Tag.BOOL,
                    right.BOOL == left.BOOL,
                );
            }

            break :blk Literal.init(Literal.Tag.BOOL, false);
        },
        else => InterpreterError.Unknown,
    };
}

fn handleInterpreterError(err: InterpreterError) void {
    const stdError = std.io.getStdErr();
    _ = switch (err) {
        InterpreterError.ExpectedBool => stdError.write("Expected a Boolean.") catch {},
        InterpreterError.ExpectedNumber => stdError.write("Expected a Number.") catch {},
        InterpreterError.Unknown => stdError.write("An unknown error occurred.") catch {},
    };
}
