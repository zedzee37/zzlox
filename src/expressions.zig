const std = @import("std");
const lexer = @import("lexer.zig");

pub const Literal = union(enum) {
    pub const Tag = @typeInfo(Literal).Union.tag_type.?;

    STRING: []const u8,
    BOOL: bool,
    NUMBER: f32,
    NIL,

    const Self = @This();

    pub fn init(comptime t: Literal.Tag, value: anytype) Self {
        return @unionInit(Self, @tagName(t), value);
    }
};

pub const Binary = struct {
    left: *const Expr,
    right: *const Expr,
    operator: lexer.Token,

    pub fn init(
        left: *const Expr,
        right: *const Expr,
        operator: lexer.Token,
    ) @This() {
        return .{
            .left = left,
            .right = right,
            .operator = operator,
        };
    }
};

pub const Unary = struct {
    operator: lexer.Token,
    right: *const Expr,

    pub fn init(operator: lexer.Token, right: *const Expr) @This() {
        return .{
            .operator = operator,
            .right = right,
        };
    }
};

pub const Expr = union(enum) {
    pub const Tag = @typeInfo(Expr).Union.tag_type.?;

    binary: Binary,
    unary: Unary,
    literal: Literal,
    grouping: *const Expr,

    const Self = @This();

    pub fn init(comptime t: Expr.Tag, value: anytype) Self {
        return @unionInit(Self, @tagName(t), value);
    }
};

test "expr" {
    _ = Expr.init(Expr.Tag.literal, Literal.init(Literal.Tag.NUMBER, 100));
}
