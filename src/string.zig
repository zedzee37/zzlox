const std = @import("std");
const Allocator = std.mem.Allocator;

pub const String = struct {
    allocator: Allocator,
    data: []u8,
    current: usize,

    const Self = @This();
    pub fn init(allocator: Allocator) !Self {
        const data = try allocator.alloc(u8, 0);
        return .{ .allocator = allocator, .data = data, .current = 0 };
    }

    pub fn from(allocator: Allocator, literal: []const u8) !Self {
        const data = try allocator.alloc(u8, literal.len);
        @memcpy(data, literal);
        return .{ .allocator = allocator, .data = data, .current = literal.len };
    }

    pub fn initSize(allocator: Allocator, size: usize) !Self {
        const data = try allocator.alloc(u8, size);
        return .{ .allocator = allocator, .data = data, .current = size };
    }

    pub fn initConcat(allocator: Allocator, s1: anytype, s2: anytype) !Self {
        const required_size = s1.len + s2.len;
        const data = try allocator.alloc(u8, required_size);

        @memcpy(data[0..s1.len], s1);
        @memcpy(data[s1.len..required_size], s2);

        return .{ .allocator = allocator, .data = data, .current = required_size };
    }

    pub fn fromFormat(
        allocator: Allocator,
        comptime format: []const u8,
        args: anytype,
    ) !Self {
        const data = try std.fmt.allocPrint(allocator, format, args);
        return .{
            .allocator = allocator,
            .data = data,
            .current = data.len,
        };
    }

    pub fn append(self: *Self, ch: u8) !void {
        if (self.current >= self.data.len) {
            var target_size = self.data.len * 2;
            if (target_size == 0) {
                target_size = 1;
            }

            self.data = try self.allocator.realloc(
                self.data,
                target_size,
            );
        }
        self.data[self.current] = ch;
        self.current += 1;
    }

    pub fn concat(self: *Self, other: String) !void {
        const end_index = self.current + other.data.len;

        if (end_index >= self.data.len) {
            const required_size = self.data.len + other.data.len;
            self.data = try self.allocator.realloc(
                self.data,
                required_size,
            );
        }

        @memcpy(
            self.data[self.current .. self.current + other.data.len],
            other.data,
        );
    }

    pub fn concatLiteral(self: *Self, other: []const u8) !void {
        const end_index = self.current + other.len;

        if (end_index >= self.data.len) {
            const required_size = self.data.len + other.len;
            self.data = try self.allocator.realloc(
                self.data,
                required_size,
            );
        }

        @memcpy(self.data[self.current .. self.current + other.len], other);
    }

    pub fn equals(self: Self, other: anytype) bool {
        return std.mem.eql(u8, self.data, other);
    }

    pub fn deinit(self: Self) void {
        self.allocator.free(self.data);
    }
};
