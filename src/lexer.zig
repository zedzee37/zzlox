const std = @import("std");
const handleLexerError = @import("logger.zig").handleLexerError;
const isDigit = std.ascii.isDigit;
const isAlpha = std.ascii.isAlphabetic;
const isAlphanumeric = std.ascii.isAlphanumeric;

pub const TokenType = union(enum) {
    pub const Tag = @typeInfo(TokenType).Union.tag_type.?;

    // Single letter tokens
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    SEMICOLON,
    SLASH,
    STAR,

    // One or two character tokens
    BANG,
    BANG_EQUAL,
    EQUAL,
    EQUAL_EQUAL,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,

    // Literals
    IDENTIFIER,
    STRING: []const u8,
    NUMBER: f32,

    // Keywords
    AND,
    FALSE,
    TRUE,
    CLASS,
    ELSE,
    FUN,
    FOR,
    IF,
    NIL,
    OR,
    PRINT,
    RETURN,
    SUPER,
    THIS,
    VAR,
    WHILE,

    EOF,

    const Self = @This();

    pub fn init(comptime t: TokenType.Tag, value: anytype) Self {
        return @unionInit(Self, @tagName(t), value);
    }
};

pub const Token = struct {
    token_type: TokenType,
    lexeme: []const u8,
    line: u32,

    const Self = @This();

    pub fn init(
        token_type: TokenType,
        lexeme: []const u8,
        line: u32,
    ) Self {
        return .{
            .token_type = token_type,
            .lexeme = lexeme,
            .line = line,
        };
    }

    pub fn string(self: Self, allocator: std.mem.Allocator) ![]u8 {
        return try std.fmt.allocPrint(allocator, "{s} {s}", .{
            @tagName(self.token_type),
            self.lexeme,
        });
    }
};

pub const LexerError = error{
    UnexpectedCharacter,
    CouldNotLexString,
    CouldNotLexNumber,
    CouldNotLexIdentifier,
    Continue,
};

const keywords = std.StaticStringMap(TokenType).initComptime(.{
    .{ "and", TokenType.init(TokenType.AND, {}) },
    .{ "class", TokenType.init(TokenType.CLASS, {}) },
    .{ "else", TokenType.init(TokenType.ELSE, {}) },
    .{ "false", TokenType.init(TokenType.FALSE, {}) },
    .{ "for", TokenType.init(TokenType.FOR, {}) },
    .{ "fun", TokenType.init(TokenType.FUN, {}) },
    .{ "if", TokenType.init(TokenType.IF, {}) },
    .{ "nil", TokenType.init(TokenType.NIL, {}) },
    .{ "or", TokenType.init(TokenType.OR, {}) },
    .{ "print", TokenType.init(TokenType.PRINT, {}) },
    .{ "return", TokenType.init(TokenType.RETURN, {}) },
    .{ "super", TokenType.init(TokenType.SUPER, {}) },
    .{ "this", TokenType.init(TokenType.THIS, {}) },
    .{ "true", TokenType.init(TokenType.TRUE, {}) },
    .{ "var", TokenType.init(TokenType.VAR, {}) },
    .{ "while", TokenType.init(TokenType.WHILE, {}) },
});

pub const Lexer = struct {
    source: []const u8,
    start: u32,
    current: u32,
    line: u32,

    const Self = @This();

    pub fn lex(allocator: std.mem.Allocator, source: []const u8) ?[]Token {
        var self: Self = .{
            .source = source,
            .start = 0,
            .current = 0,
            .line = 1,
        };
        var tokens = std.ArrayList(Token).init(allocator);
        while (!self.isAtEnd()) {
            self.start = self.current;

            const next = self.lexNext() catch |err| {
                switch (err) {
                    LexerError.Continue => {},
                    else => {
                        handleLexerError(
                            self.line,
                            err,
                        ) catch {};
                        return null;
                    },
                }
                continue;
            };
            tokens.append(next) catch return null;
        }

        var empty = "".*;
        tokens.append(Token.init(
            TokenType.init(TokenType.Tag.EOF, {}),
            &empty,
            self.line,
        )) catch {
            return null;
        };
        return tokens.toOwnedSlice() catch null;
    }

    fn lexNext(self: *Self) !Token {
        const c = self.advance();
        return switch (c) {
            '(' => self.emptyToken(TokenType.Tag.LEFT_PAREN),
            ')' => self.emptyToken(TokenType.Tag.RIGHT_PAREN),
            '{' => self.emptyToken(TokenType.Tag.LEFT_BRACE),
            '}' => self.emptyToken(TokenType.Tag.RIGHT_BRACE),
            ',' => self.emptyToken(TokenType.Tag.COMMA),
            '.' => self.emptyToken(TokenType.Tag.DOT),
            '-' => self.emptyToken(TokenType.Tag.MINUS),
            '+' => self.emptyToken(TokenType.Tag.PLUS),
            ';' => self.emptyToken(TokenType.Tag.SEMICOLON),
            '*' => self.emptyToken(TokenType.Tag.STAR),

            '\n' => {
                self.line += 1;
                return LexerError.Continue;
            },
            ' ' => LexerError.Continue,
            '\r' => LexerError.Continue,
            '\t' => LexerError.Continue,

            '!' => self.emptyTokenMatch(
                '=',
                TokenType.Tag.BANG_EQUAL,
                TokenType.Tag.BANG,
            ),
            '=' => self.emptyTokenMatch(
                '=',
                TokenType.Tag.EQUAL_EQUAL,
                TokenType.Tag.EQUAL,
            ),
            '<' => self.emptyTokenMatch(
                '=',
                TokenType.Tag.LESS_EQUAL,
                TokenType.Tag.LESS,
            ),
            '>' => self.emptyTokenMatch(
                '=',
                TokenType.Tag.GREATER_EQUAL,
                TokenType.Tag.GREATER,
            ),

            '/' => blk: {
                if (self.match('/')) {
                    while (!self.isAtEnd() and self.getCurrent() != '\n') {
                        self.current += 1;
                    }
                    break :blk LexerError.Continue;
                } else if (self.match('*')) {
                    while (!self.isAtEnd()) {
                        self.current += 1;
                        if (self.getCurrent() == '*' and self.getNext() == '/') {
                            self.current += 2;
                            break :blk LexerError.Continue;
                        }
                    }
                }
                break :blk self.emptyToken(TokenType.Tag.SLASH);
            },

            '"' => self.stringToken(),
            else => blk: {
                if (isDigit(c)) {
                    break :blk self.numberToken();
                } else if (isAlpha(c)) {
                    break :blk self.identifierToken();
                }
                return LexerError.UnexpectedCharacter;
            },
        };
    }

    fn advance(self: *Self) u8 {
        const current = self.getCurrent();
        self.current += 1;
        return current;
    }

    fn emptyTokenMatch(
        self: *Self,
        comptime to_match: u8,
        comptime token_var1: TokenType.Tag,
        comptime token_var2: TokenType.Tag,
    ) Token {
        if (self.match(to_match)) {
            return self.emptyToken(token_var1);
        }
        return self.emptyToken(token_var2);
    }

    fn emptyToken(self: Self, comptime token_variant: TokenType.Tag) Token {
        return self.token(TokenType.init(token_variant, {}));
    }

    fn stringToken(self: *Self) !Token {
        while (self.getCurrent() != '"' and !self.isAtEnd()) {
            if (self.getCurrent() == '\n') {
                self.line += 1;
            }
            self.current += 1;
        }

        if (self.isAtEnd()) {
            return LexerError.CouldNotLexString;
        }

        self.current += 1;
        const str = self.source[self.start + 1 .. self.current - 1];
        return self.token(TokenType.init(TokenType.Tag.STRING, str));
    }

    fn numberToken(self: *Self) !Token {
        while (!self.isAtEnd() and isDigit(self.getCurrent())) {
            self.current += 1;
        }

        if (!self.isAtEnd() and
            self.getCurrent() == '.' and
            isDigit(self.getNext()))
        {
            self.current += 1;

            while (!self.isAtEnd() and isDigit(self.getCurrent())) {
                self.current += 1;
            }
        }

        const str = self.source[self.start..self.current];
        const num = std.fmt.parseFloat(f32, str) catch {
            return LexerError.CouldNotLexNumber;
        };
        return self.token(TokenType.init(TokenType.Tag.NUMBER, num));
    }

    fn identifierToken(self: *Self) !Token {
        while (!self.isAtEnd() and isAlphanumeric(self.getCurrent())) {
            self.current += 1;
        }

        const str = self.source[self.start..self.current];
        if (keywords.has(str)) {
            return self.token(keywords.get(str).?);
        }

        return self.emptyToken(TokenType.Tag.IDENTIFIER);
    }

    fn token(self: Self, token_type: TokenType) Token {
        const str = self.source[self.start..self.current];
        return Token.init(token_type, str, self.line);
    }

    fn match(self: *Self, expected: u8) bool {
        if (self.isAtEnd() or self.getCurrent() != expected) {
            return false;
        }
        self.current += 1;
        return true;
    }

    inline fn isAtEnd(self: Self) bool {
        return self.current >= self.source.len;
    }

    inline fn getCurrent(self: Self) u8 {
        return self.source[self.current];
    }

    inline fn getNext(self: Self) u8 {
        return self.source[self.current + 1];
    }
};
