const std = @import("std");

const lexer = @import("lexer.zig");
const Token = lexer.Token;
const TokenType = lexer.TokenType;

const report = @import("logger.zig").report;

const expressions = @import("expressions.zig");
const Expr = expressions.Expr;

// TODO: Make Parser take in an ArenaAllocator, use heap allocation
// for expressions, and make parser only have the parse method as pub

pub const ParserError = error{
    CouldNotParse,
    ExpectedCharacter,
};

pub const Parser = struct {
    tokens: []Token,
    current: usize,

    const Self = @This();

    pub fn init(tokens: []Token) Self {
        return .{ .tokens = tokens, .current = 0 };
    }

    pub fn parse(self: *Self) ?Expr {
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

    fn expression(self: *Self) ParserError!Expr {
        return self.equality();
    }

    fn equality(self: *Self) !Expr {
        var expr = try self.comparison();

        while (self.match(&.{
            TokenType.Tag.BANG_EQUAL,
            TokenType.Tag.EQUAL_EQUAL,
        })) {
            const operator = self.getPrevious();
            const right = try self.comparison();

            expr = Expr.init(Expr.Tag.binary, expressions.Binary.init(
                &expr,
                &right,
                operator,
            ));
        }

        return expr;
    }

    fn comparison(self: *Self) !Expr {
        var expr = try self.term();

        while (self.match(&.{
            TokenType.Tag.GREATER,
            TokenType.Tag.GREATER_EQUAL,
            TokenType.Tag.LESS,
            TokenType.Tag.LESS_EQUAL,
        })) {
            const operator = self.getPrevious();
            const right = try self.term();

            expr = Expr.init(Expr.Tag.binary, expressions.Binary.init(
                &expr,
                &right,
                operator,
            ));
        }

        return expr;
    }

    fn term(self: *Self) !Expr {
        var expr = try self.factor();

        while (self.match(&.{ TokenType.Tag.PLUS, TokenType.Tag.MINUS })) {
            const operator = self.getPrevious();
            const right = try self.factor();

            expr = Expr.init(Expr.Tag.binary, expressions.Binary.init(
                &expr,
                &right,
                operator,
            ));
        }

        return expr;
    }

    fn factor(self: *Self) !Expr {
        var expr = try self.unary();

        while (self.match(&.{ TokenType.Tag.STAR, TokenType.Tag.SLASH })) {
            const operator = self.getPrevious();
            const right = try self.unary();

            expr = Expr.init(Expr.Tag.binary, expressions.Binary.init(
                &expr,
                &right,
                operator,
            ));
        }

        return expr;
    }

    fn unary(self: *Self) !Expr {
        if (self.match(&.{ TokenType.Tag.BANG, TokenType.Tag.MINUS })) {
            const operator = self.getPrevious();
            const right = try self.unary();

            return Expr.init(Expr.Tag.unary, expressions.Unary.init(
                operator,
                &right,
            ));
        }

        return self.primary();
    }

    fn primary(self: *Self) !Expr {
        const literalMaybe = if (self.match(&.{TokenType.Tag.FALSE}))
            expressions.Literal.init(expressions.Literal.Tag.BOOL, false)
        else if (self.match(&.{TokenType.Tag.TRUE}))
            expressions.Literal.init(expressions.Literal.Tag.BOOL, true)
        else if (self.match(&.{TokenType.Tag.NIL}))
            expressions.Literal.init(expressions.Literal.Tag.NIL, {})
        else
            null;

        if (literalMaybe) |literal| {
            return Expr.init(Expr.Tag.literal, literal);
        }

        if (self.match(&.{ TokenType.Tag.NUMBER, TokenType.Tag.STRING })) {
            return Expr.init(
                Expr.Tag.literal,
                Parser.asLiteral(self.getPrevious()),
            );
        }

        if (self.match(&.{TokenType.Tag.LEFT_PAREN})) {
            const expr = try self.expression();
            _ = try self.consume(
                TokenType.Tag.RIGHT_PAREN,
                "Expect ')' after expression.",
            );
            return Expr.init(Expr.Tag.grouping, &expr);
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

    inline fn asLiteral(token: Token) expressions.Literal {
        return switch (token.token_type) {
            .STRING => expressions.Literal.init(
                expressions.Literal.Tag.STRING,
                token.token_type.STRING,
            ),
            .NUMBER => expressions.Literal.init(
                expressions.Literal.Tag.NUMBER,
                token.token_type.NUMBER,
            ),
            else => expressions.Literal.init(expressions.Literal.Tag.NIL, {}),
        };
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
