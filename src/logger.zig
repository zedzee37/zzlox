const std = @import("std");
const LexerError = @import("lexer.zig").LexerError;

pub fn reportError(line: u32, msg: []const u8) !void {
    try report(line, "", msg);
}

pub fn handleLexerError(line: u32, err: LexerError) !void {
    try switch (err) {
        LexerError.UnexpectedCharacter => reportError(
            line,
            "Unexpected Character.",
        ),
        LexerError.CouldNotLexNumber => reportError(
            line,
            "Could not parse number.",
        ),
        LexerError.CouldNotLexString => reportError(
            line,
            "Could not parse string.",
        ),
        LexerError.CouldNotLexIdentifier => reportError(
            line,
            "Could not parse identifier.",
        ),
        else => {},
    };
}

pub fn report(line: u32, where: anytype, message: []const u8) !void {
    const stdout = std.io.getStdOut().writer();
    try stdout.print("[line {d}] Error{s}: {s}\n", .{ line, where, message });
}
