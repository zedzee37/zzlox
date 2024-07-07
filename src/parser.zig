const std = @import("std");

const lexer = @import("lexer.zig");
const Token = lexer.Token;
const TokenType = lexer.TokenType;

const report = @import("logger.zig").report;

const expressions = @import("expressions.zig");
const Expr = expressions.Expr;

pub const ParserError = error{
    CouldNotParse,
    ExpectedCharacter,
    CouldNotAllocate,
};

pub const Parser = struct {
    tokens: []Token,
    current: usize,
    allocator: std.mem.Allocator,

    const Self = @This();

    fn init(allocator: std.mem.Allocator, tokens: []Token) Self {
        return .{
            .tokens = tokens,
            .current = 0,
            .allocator = allocator,
        };
    }

    pub fn parse(arena: *std.heap.ArenaAllocator, tokens: []Token) ?*Expr {
        var self = Self.init(arena.allocator(), tokens);
        return self.expression() catch null;
    }

    fn synchronize(self: *Self) void {
        self.current += 1;

        switch (self.getCurrent().token_type) {
            .CLASS => {},
            .FUN => {},
            .VAR => {},
            .FOR => {},
            .IF => {},
            .WHILE => {},
            .PRINT => {},
            .RETURN => return,
        }

        self.current += 1;
    }

    fn expression(self: *Self) ParserError!*Expr {
        return self.equality();
    }

    fn equality(self: *Self) !*Expr {
        var expr = try self.comparison();

        while (self.match(&.{
            TokenType.Tag.BANG_EQUAL,
            TokenType.Tag.EQUAL_EQUAL,
        })) {
            const operator = self.getPrevious();
            const right = try self.comparison();

            const binary = self.allocator.create(expressions.Binary) catch {
                return ParserError.CouldNotAllocate;
            };
            binary.* = expressions.Binary.init(expr, right, operator);

            expr = self.allocator.create(Expr) catch {
                return ParserError.CouldNotAllocate;
            };
            expr.* = Expr.init(Expr.Tag.binary, binary);
        }

        return expr;
    }

    fn comparison(self: *Self) !*Expr {
        var expr = try self.term();

        while (self.match(&.{
            TokenType.Tag.GREATER,
            TokenType.Tag.GREATER_EQUAL,
            TokenType.Tag.LESS,
            TokenType.Tag.LESS_EQUAL,
        })) {
            const operator = self.getPrevious();
            const right = try self.term();

            const binary = self.allocator.create(expressions.Binary) catch {
                return ParserError.CouldNotAllocate;
            };
            binary.* = expressions.Binary.init(expr, right, operator);

            expr = self.allocator.create(Expr) catch {
                return ParserError.CouldNotAllocate;
            };
            expr.* = Expr.init(Expr.Tag.binary, binary);
        }

        return expr;
    }

    fn term(self: *Self) !*Expr {
        var expr = try self.factor();

        while (self.match(&.{ TokenType.Tag.PLUS, TokenType.Tag.MINUS })) {
            const operator = self.getPrevious();
            const right = try self.factor();

            const binary = self.allocator.create(expressions.Binary) catch {
                return ParserError.CouldNotAllocate;
            };
            binary.* = expressions.Binary.init(expr, right, operator);

            expr = self.allocator.create(Expr) catch {
                return ParserError.CouldNotAllocate;
            };
            expr.* = Expr.init(Expr.Tag.binary, binary);
        }

        return expr;
    }

    fn factor(self: *Self) !*Expr {
        var expr = try self.unary();

        while (self.match(&.{ TokenType.Tag.STAR, TokenType.Tag.SLASH })) {
            const operator = self.getPrevious();
            const right = try self.unary();

            const binary = self.allocator.create(expressions.Binary) catch {
                return ParserError.CouldNotAllocate;
            };
            binary.* = expressions.Binary.init(expr, right, operator);

            expr = self.allocator.create(Expr) catch {
                return ParserError.CouldNotAllocate;
            };
            expr.* = Expr.init(Expr.Tag.binary, binary);
        }

        return expr;
    }

    fn unary(self: *Self) !*Expr {
        if (self.match(&.{ TokenType.Tag.BANG, TokenType.Tag.MINUS })) {
            const operator = self.getPrevious();
            const right = try self.unary();

            const u = self.allocator.create(expressions.Unary) catch {
                return ParserError.CouldNotAllocate;
            };
            u.* = expressions.Unary.init(operator, right);

            const expr = self.allocator.create(Expr) catch {
                return ParserError.CouldNotAllocate;
            };
            expr.* = Expr.init(Expr.Tag.unary, u);
        }

        return self.primary();
    }

    fn primary(self: *Self) !*Expr {
        if (self.match(&.{TokenType.Tag.FALSE})) {
            const expr = self.allocator.create(Expr) catch {
                return ParserError.CouldNotAllocate;
            };
            const literal = self.allocator.create(expressions.Literal) catch {
                return ParserError.CouldNotAllocate;
            };

            literal.* = expressions.Literal.init(
                expressions.Literal.Tag.BOOL,
                false,
            );

            expr.* = Expr.init(Expr.Tag.literal, literal);
            return expr;
        } else if (self.match(&.{TokenType.Tag.TRUE})) {
            const expr = self.allocator.create(Expr) catch {
                return ParserError.CouldNotAllocate;
            };
            const literal = self.allocator.create(expressions.Literal) catch {
                return ParserError.CouldNotAllocate;
            };

            literal.* = expressions.Literal.init(
                expressions.Literal.Tag.BOOL,
                false,
            );

            expr.* = Expr.init(Expr.Tag.literal, literal);
            return expr;
        } else if (self.match(&.{TokenType.Tag.NIL})) {
            const expr = self.allocator.create(Expr) catch {
                return ParserError.CouldNotAllocate;
            };
            const literal = self.allocator.create(expressions.Literal) catch {
                return ParserError.CouldNotAllocate;
            };

            literal.* = expressions.Literal.init(
                expressions.Literal.Tag.NIL,
                {},
            );

            expr.* = Expr.init(Expr.Tag.literal, literal);
            return expr;
        }

        if (self.match(&.{ TokenType.Tag.NUMBER, TokenType.Tag.STRING })) {
            const expr = self.allocator.create(Expr) catch {
                return ParserError.CouldNotAllocate;
            };
            const literal = try self.asLiteral(self.getPrevious());
            expr.* = Expr.init(Expr.Tag.literal, literal);
            return expr;
        }

        if (self.match(&.{TokenType.Tag.LEFT_PAREN})) {
            const expr = try self.expression();
            _ = try self.consume(
                TokenType.Tag.RIGHT_PAREN,
                "Expect ')' after expression.",
            );

            const group = self.allocator.create(Expr) catch {
                return ParserError.CouldNotAllocate;
            };
            group.* = Expr.init(Expr.Tag.grouping, expr);
            return group;
        }
        return ParserError.CouldNotParse;
    }

    fn consume(self: *Self, tag: TokenType.Tag, msg: []const u8) !Token {
        if (self.check(tag)) {
            return self.advance();
        }

        Parser.handleParserError(self.getCurrent(), msg);
        return ParserError.ExpectedCharacter;
    }

    fn match(self: *Self, types: []const TokenType.Tag) bool {
        for (types) |tok| {
            if (self.check(tok)) {
                self.current += 1;
                return true;
            }
        }

        return false;
    }

    inline fn check(self: Self, token_type: TokenType.Tag) bool {
        if (self.isAtEnd()) {
            return false;
        }
        // Check if the type (not the value) is equal to the other type
        return @as(TokenType.Tag, self.getCurrent().token_type) == token_type;
    }

    inline fn advance(self: *Self) Token {
        const current = self.getCurrent();
        self.current += 1;
        return current;
    }

    inline fn getPrevious(self: Self) Token {
        return self.tokens[self.current - 1];
    }

    inline fn getCurrent(self: Self) Token {
        return self.tokens[self.current];
    }

    inline fn isAtEnd(self: Self) bool {
        return self.current >= self.tokens.len;
    }

    inline fn asLiteral(self: Self, token: Token) !*expressions.Literal {
        const literal: *expressions.Literal = self.allocator.create(
            expressions.Literal,
        ) catch {
            return ParserError.CouldNotAllocate;
        };

        literal.* = switch (token.token_type) {
            .STRING => expressions.Literal.init(
                expressions.Literal.Tag.STRING,
                token.token_type.STRING,
            ),
            .NUMBER => expressions.Literal.init(
                expressions.Literal.Tag.NUMBER,
                token.token_type.NUMBER,
            ),
            else => return ParserError.CouldNotParse,
        };

        return literal;
    }

    inline fn handleParserError(
        token: Token,
        msg: []const u8,
    ) void {
        if (@as(TokenType.Tag, token.token_type) == TokenType.Tag.EOF) {
            report(token.line, " at end", msg) catch return;
        } else {
            var buf: []u8 = undefined;
            buf = std.fmt.bufPrint(
                buf,
                "at '{s}'",
                .{token.lexeme},
            ) catch return;
            report(token.line, buf, msg) catch return;
        }
    }
};

test "test enums" {
    const allocator = std.testing.allocator;
    const expr = try allocator.create(Expr);
    defer allocator.destroy(expr);

    const literal = try allocator.create(expressions.Literal);
    defer allocator.destroy(literal);

    literal.* = .{ .NUMBER = 10 };
    expr.* = .{ .literal = literal };

    std.debug.print("{d}", .{expr.literal.NUMBER});
}
