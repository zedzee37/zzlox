const std = @import("std");
const Allocator = std.mem.Allocator;

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
};

pub const LexerPayload = struct {
    where: ?usize,
    line: ?usize,

    fn init() @This() {
        return .{ .where = null, .line = null };
    }
};

pub const LexerError = error{
    OutOfRange,
};

pub fn lex(
    allocator: Allocator,
    source: []const u8,
    payload: *LexerPayload,
) ![]Token {
    var self = Lexer{
        .allocator = allocator,
        .source = source,
        .payload = payload,
    };
}

const Lexer = struct {
    allocator: Allocator,
    source: []const u8,
    current: usize,
    payload: *LexerPayload,

    const Self = @This();

    fn nextToken(self: *Self) !Token {
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

    inline fn isAtEnd(self: Self) bool {
        return self.current >= self.source.len;
    }
};
