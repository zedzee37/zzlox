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

pub const ParserPayload = struct {
    expected: ?TokenTag,
    found: ?TokenTag,
    found_lexeme: ?[]const u8,
    line: ?usize,
    length: ?usize,
    idx: ?isize,

    pub fn init() @This() {
        return .{
            .expected = null,
            .found = null,
            .found_lexeme = null,
            .line = null,
            .length = null,
            .idx = null,
        };
    }
};

pub const ParserError = error{
    UnexpectedToken,
    ExpectedToken,
    CouldNotParse,
    OutOfBoundsError,
    CouldNotAllocate,
};

pub fn parse(
    arena: *ArenaAllocator,
    source: []Token,
    payload: *ParserPayload,
) !*Expr {
    var self = Parser.init(arena, payload, source);
    return self.expression();
}

const Parser = struct {
    allocator: Allocator,
    payload: *ParserPayload,
    source: []Token,
    current: usize,

    const Self = @This();

    fn init(
        arena: *ArenaAllocator,
        payload: *ParserPayload,
        source: []Token,
    ) Self {
        return .{
            .allocator = arena.allocator(),
            .payload = payload,
            .source = source,
            .current = 0,
        };
    }

    fn synchronize(self: *Self) !void {
        self.current += 1;

        while (!self.isAtEnd()) {
            const prev = try self.previous();
            if (prev.asTag() == .SEMICOLON) {
                return;
            }

            const current = try self.get();
            switch (current) {
                .CLASS => {},
                .FUN => {},
                .VAR => {},
                .FOR => {},
                .IF => {},
                .WHILE => {},
                .PRINT => {},
                .RETURN => return,
            }
        }

        self.current += 1;
    }

    fn expression(self: *Self) ParserError!*Expr {
        return self.equality();
    }

    fn equality(self: *Self) !*Expr {
        var expr = try self.comparison();
        while (try self.match(&.{ .BANG_EQUAL, .EQUAL_EQUAL })) {
            const operator = try self.previous();
            const right = try self.comparison();

            const binary = self.allocator.create(Binary) catch {
                return ParserError.CouldNotAllocate;
            };
            binary.* = Binary.init(expr, right, operator);

            expr = self.allocator.create(Expr) catch {
                return ParserError.CouldNotAllocate;
            };
            expr.* = Expr.init(.binary, binary);
        }
        return expr;
    }

    fn comparison(self: *Self) !*Expr {
        var expr = try self.term();
        while (try self.match(&.{ .GREATER, .GREATER_EQUAL, .LESS, .LESS_EQUAL })) {
            const operator = try self.previous();
            const right = try self.term();

            const binary = self.allocator.create(Binary) catch {
                return ParserError.CouldNotAllocate;
            };
            binary.* = Binary.init(expr, right, operator);

            expr = self.allocator.create(Expr) catch {
                return ParserError.CouldNotAllocate;
            };
            expr.* = Expr.init(.binary, binary);
        }
        return expr;
    }

    fn term(self: *Self) !*Expr {
        var expr = try self.factor();
        while (try self.match(&.{ .MINUS, .PLUS })) {
            const operator = try self.previous();
            const right = try self.factor();

            const binary = self.allocator.create(Binary) catch {
                return ParserError.CouldNotAllocate;
            };
            binary.* = Binary.init(expr, right, operator);

            expr = self.allocator.create(Expr) catch {
                return ParserError.CouldNotAllocate;
            };
            expr.* = Expr.init(.binary, binary);
        }
        return expr;
    }

    fn factor(self: *Self) !*Expr {
        var expr = try self.unary();
        while (try self.match(&.{ .STAR, .SLASH })) {
            const operator = try self.previous();
            const right = try self.unary();

            const binary = self.allocator.create(Binary) catch {
                return ParserError.CouldNotAllocate;
            };
            binary.* = Binary.init(expr, right, operator);

            expr = self.allocator.create(Expr) catch {
                return ParserError.CouldNotAllocate;
            };
            expr.* = Expr.init(.binary, binary);
        }
        return expr;
    }

    fn unary(self: *Self) !*Expr {
        if (try self.match(&.{ .BANG, .MINUS })) {
            const operator = try self.previous();
            const right = try self.unary();

            const u = self.allocator.create(Unary) catch {
                return ParserError.CouldNotAllocate;
            };
            u.* = Unary.init(operator, right);

            const expr = self.allocator.create(Expr) catch {
                return ParserError.CouldNotAllocate;
            };
            expr.* = Expr.init(.unary, u);
            return expr;
        }

        return self.primary();
    }

    fn primary(self: *Self) !*Expr {
        const literalRef: *Literal = self.allocator.create(Literal) catch {
            return ParserError.CouldNotAllocate;
        };
        var literalMaybe: ?Literal = null;
        literalMaybe = if (try self.match(&.{.TRUE}))
            Literal.init(.BOOL, true)
        else if (try self.match(&.{.FALSE}))
            Literal.init(.BOOL, false)
        else if (try self.match(&.{.NIL}))
            Literal.init(.NIL, {})
        else
            null;

        if (try self.match(&.{ TokenTag.NUMBER, TokenTag.STRING })) {
            const prev = try self.previous();
            literalMaybe = switch (prev.token_type) {
                .NUMBER => |num| Literal.init(.NUMBER, num),
                .STRING => |str| Literal.init(.STRING, str),
                else => null,
            };
        }

        if (literalMaybe) |literal| {
            literalRef.* = literal;
            const expr = self.allocator.create(Expr) catch {
                return ParserError.CouldNotAllocate;
            };
            expr.* = Expr.init(.literal, literalRef);
            return expr;
        }

        if (try self.match(&.{.LEFT_PAREN})) {
            const expr = try self.expression();
            try self.consume(.RIGHT_PAREN);

            const group = self.allocator.create(Expr) catch {
                return ParserError.CouldNotAllocate;
            };
            group.* = Expr.init(.grouping, expr);

            return group;
        }

        return ParserError.CouldNotParse;
    }

    inline fn consume(self: *Self, expected: TokenType) ParserError!void {
        if (try self.check(expected)) {
            self.current += 1;
            return;
        }

        const current = try self.get();
        self.payload.expected = expected;
        self.payload.found = current.asTag();
        self.payload.line = current.line;
        self.payload.found_lexeme = current.lexeme;
        return ParserError.ExpectedToken;
    }

    inline fn match(self: *Self, to_match: []const TokenTag) !bool {
        const cur = try self.get();
        const tok = cur.asTag();
        for (to_match) |tag| {
            if (tok == tag) {
                self.current += 1;
                return true;
            }
        }
        return false;
    }

    inline fn check(self: *Self, expected: TokenType) !bool {
        const tok = try self.get();
        return tok.asTag() == expected;
    }

    inline fn get(self: *Self) !Token {
        if (self.isAtEnd()) {
            self.payload.length = self.source.len;
            self.payload.idx = @as(isize, @intCast(self.current));
            return ParserError.OutOfBoundsError;
        }
        return self.source[self.current];
    }

    inline fn previous(self: *Self) !Token {
        if (self.current == 0 or self.current - 1 >= self.source.len) {
            self.payload.length = self.source.len;
            self.payload.idx = @as(isize, @intCast(self.current)) - 1;
            return ParserError.OutOfBoundsError;
        }
        return self.source[self.current - 1];
    }

    inline fn isAtEnd(self: Self) bool {
        return self.current >= self.source.len;
    }
};
