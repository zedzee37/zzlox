const std = @import("std");
const Allocator = std.mem.Allocator;

const isDigit = std.ascii.isDigit;
const isAlpha = std.ascii.isAlphabetic;
const isAlphanumeric = std.ascii.isAlphanumeric;

const String = @import("string.zig").String;

pub const TokenTag = @typeInfo(TokenType).Union.tag_type.?;

pub const TokenType = union(enum) {
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

    pub fn init(comptime t: TokenTag, value: anytype) Self {
        return @unionInit(Self, @tagName(t), value);
    }

    pub fn initEmpty(comptime t: TokenTag) Self {
        return @unionInit(Self, @tagName(t), {});
    }

    pub fn toTag(self: Self) TokenTag {
        return @as(TokenTag, self);
    }
};

pub const Token = struct {
    token_type: TokenType,
    line: usize,
    lexeme: []const u8,

    pub fn init(
        token_type: TokenType,
        line: usize,
        lexeme: []const u8,
    ) @This() {
        return .{
            .token_type = token_type,
            .line = line,
            .lexeme = lexeme,
        };
    }

    pub fn string(self: @This(), allocator: Allocator) !String {
        return String.fromFormat(allocator, "{s} {s}", .{
            @tagName(self.token_type),
            self.lexeme,
        });
    }

    pub inline fn asTag(self: @This()) TokenTag {
        return @as(TokenTag, self.token_type);
    }
};

pub const LexerPayload = struct {
    where: ?usize,
    line: ?usize,
    expected: ?u8,
    found: ?u8,

    pub fn init() @This() {
        return .{
            .where = null,
            .line = null,
            .expected = null,
            .found = null,
        };
    }
};

pub const LexerError = error{
    OutOfRange,
    CouldNotParseNumber,
    ExpectedCharacter,
    UnexpectedCharacter,
    CouldNotAddToken,
    OutOfMemory,
};

pub fn lex(
    allocator: Allocator,
    source: []const u8,
    payload: *LexerPayload,
) LexerError![]Token {
    var tokens = std.ArrayList(Token).init(allocator);

    var self = Lexer{
        .allocator = allocator,
        .source = source,
        .payload = payload,
        .current = 0,
        .start = 0,
        .line = 1,
    };

    while (!self.isAtEnd()) {
        const tokMaybe = self.nextToken();
        if (tokMaybe) |tok| {
            tokens.append(try tok) catch return LexerError.CouldNotAddToken;
        }
    }

    tokens.append(Token.init(
        TokenType.initEmpty(TokenType.EOF),
        self.line,
        "",
    )) catch return LexerError.CouldNotAddToken;

    return tokens.toOwnedSlice() catch return LexerError.OutOfMemory;
}

const Lexer = struct {
    allocator: Allocator,
    payload: *LexerPayload,
    source: []const u8,
    current: usize,
    start: usize,
    line: usize,

    const Self = @This();
    const keywords = std.StaticStringMap(TokenType).initComptime(.{
        .{ "and", TokenType.initEmpty(TokenType.AND) },
        .{ "class", TokenType.initEmpty(TokenType.CLASS) },
        .{ "else", TokenType.initEmpty(TokenType.ELSE) },
        .{ "false", TokenType.initEmpty(TokenType.FALSE) },
        .{ "for", TokenType.initEmpty(TokenType.FOR) },
        .{ "fun", TokenType.initEmpty(TokenType.FUN) },
        .{ "if", TokenType.initEmpty(TokenType.IF) },
        .{ "nil", TokenType.initEmpty(TokenType.NIL) },
        .{ "or", TokenType.initEmpty(TokenType.OR) },
        .{ "print", TokenType.initEmpty(TokenType.PRINT) },
        .{ "return", TokenType.initEmpty(TokenType.RETURN) },
        .{ "super", TokenType.initEmpty(TokenType.SUPER) },
        .{ "this", TokenType.initEmpty(TokenType.THIS) },
        .{ "true", TokenType.initEmpty(TokenType.TRUE) },
        .{ "var", TokenType.initEmpty(TokenType.VAR) },
        .{ "while", TokenType.initEmpty(TokenType.WHILE) },
    });

    fn nextToken(self: *Self) ?LexerError!Token {
        self.start = self.current;
        const c = try self.advance();

        return switch (c) {
            '{' => self.emptyToken(TokenType.LEFT_BRACE),
            '}' => self.emptyToken(TokenType.RIGHT_BRACE),
            '(' => self.emptyToken(TokenType.LEFT_PAREN),
            ')' => self.emptyToken(TokenType.RIGHT_PAREN),
            ',' => self.emptyToken(TokenType.COMMA),
            '.' => self.emptyToken(TokenType.DOT),
            '-' => self.emptyToken(TokenType.MINUS),
            '+' => self.emptyToken(TokenType.PLUS),
            ';' => self.emptyToken(TokenType.SEMICOLON),
            '*' => self.emptyToken(TokenType.STAR),

            '\n' => blk: {
                self.current += 1;
                break :blk if (!self.isAtEnd()) self.nextToken() else null;
            },
            '\t' => if (!self.isAtEnd()) self.nextToken() else null,
            ' ' => if (!self.isAtEnd()) self.nextToken() else null,
            '\r' => if (!self.isAtEnd()) self.nextToken() else null,

            '!' => self.matchElse('=', TokenType.BANG_EQUAL, TokenType.BANG),
            '=' => self.matchElse('=', TokenType.EQUAL_EQUAL, TokenType.EQUAL),
            '<' => self.matchElse('=', TokenType.LESS_EQUAL, TokenType.LESS),
            '>' => self.matchElse(
                '=',
                TokenType.GREATER_EQUAL,
                TokenType.GREATER,
            ),
            '/' => blk: {
                if (try self.match('/')) {
                    while (!self.isAtEnd() and try self.get() != '\n') {
                        self.current += 1;
                    }
                    break :blk if (!self.isAtEnd())
                        self.nextToken()
                    else
                        null;
                } else if (try self.match('*')) {
                    while (!self.isAtEnd()) {
                        self.current += 1;
                        if (try self.get() == '*' and
                            try self.getNext() == '/')
                        {
                            self.current += 2;
                            break :blk if (!self.isAtEnd())
                                self.nextToken()
                            else
                                null;
                        }
                    }
                }
                break :blk self.emptyToken(TokenType.SLASH);
            },

            '"' => self.stringToken(),

            else => blk: {
                if (isDigit(c)) {
                    break :blk self.numberToken();
                } else if (isAlpha(c)) {
                    break :blk self.identifierToken();
                }
                self.payload.line = self.line;
                self.payload.where = self.current - 1;
                self.payload.found = c;
                break :blk LexerError.UnexpectedCharacter;
            },
        };
    }

    fn matchElse(
        self: *Self,
        comptime expected: u8,
        comptime pass: TokenTag,
        comptime failure: TokenTag,
    ) !Token {
        return if (try self.match(expected))
            self.emptyToken(pass)
        else
            self.emptyToken(failure);
    }

    fn stringToken(self: *Self) !Token {
        while (!self.isAtEnd() and try self.get() != '"') {
            if (self.current == self.source.len - 1) {
                self.payload.expected = '"';
                self.payload.line = self.line;
                self.payload.where = self.current;
                self.payload.found = try self.get();
                return LexerError.ExpectedCharacter;
            }
            self.current += 1;
        }

        self.current += 1;

        const str = self.source[self.start + 1 .. self.current - 1];
        const tag = TokenType.init(TokenType.STRING, str);
        return self.token(tag);
    }

    fn numberToken(self: *Self) !Token {
        while (!self.isAtEnd() and isDigit(try self.get())) {
            self.current += 1;
        }

        if (!self.isAtEnd() and try self.match('.')) {
            while (!self.isAtEnd() and isDigit(try self.get())) {
                self.current += 1;
            }
        }

        const numStr = self.source[self.start..self.current];
        const parsed = std.fmt.parseFloat(f32, numStr) catch {
            self.payload.line = self.line;
            return LexerError.CouldNotParseNumber;
        };
        return self.token(TokenType.init(TokenType.NUMBER, parsed));
    }

    fn identifierToken(self: *Self) !Token {
        while (!self.isAtEnd() and isAlphanumeric(try self.get())) {
            self.current += 1;
        }

        const str = self.source[self.start..self.current];
        if (Self.keywords.has(str)) {
            return self.token(Self.keywords.get(str).?);
        }
        return self.emptyToken(TokenType.IDENTIFIER);
    }

    fn emptyToken(self: Self, comptime tag: TokenTag) !Token {
        return self.token(TokenType.initEmpty(tag));
    }

    fn token(self: Self, token_type: TokenType) !Token {
        if (self.current - 1 >= self.source.len) {
            return LexerError.OutOfRange;
        }
        const lexeme = self.source[self.start..self.current];
        return Token.init(token_type, self.line, lexeme);
    }

    fn match(self: *Self, expected: u8) !bool {
        if (self.isAtEnd() or try self.get() != expected) {
            return false;
        }
        self.current += 1;
        return true;
    }

    inline fn advance(self: *Self) !u8 {
        const c = try self.get();
        self.current += 1;
        return c;
    }

    inline fn get(self: Self) !u8 {
        if (self.isAtEnd()) {
            return LexerError.OutOfRange;
        }
        return self.source[self.current];
    }

    inline fn getNext(self: Self) !u8 {
        if (self.current + 1 >= self.source.len) {
            return LexerError.OutOfRange;
        }
        return self.source[self.current + 1];
    }

    inline fn isAtEnd(self: Self) bool {
        return self.current >= self.source.len;
    }
};
