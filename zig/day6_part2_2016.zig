
const std = @import("std");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(content);

    var lines = std.mem.tokenize(u8, content, "\n");
    var messages = std.ArrayList([]const u8).init(allocator);
    defer messages.deinit();

    while (lines.next()) |line| {
        try messages.append(line);
    }

    const original_message = try getOriginalMessage(allocator, messages.items);
    defer allocator.free(original_message);

    const stdout = std.io.getStdOut().writer();
    try stdout.print("{s}\n", .{original_message});
}

fn getOriginalMessage(allocator: std.mem.Allocator, messages: []const []const u8) ![]u8 {
    if (messages.len == 0) return allocator.alloc(u8, 0);

    const message_length = messages[0].len;
    var count = try allocator.alloc(std.AutoHashMap(u8, usize), message_length);
    defer {
        for (count) |*map| map.deinit();
        allocator.free(count);
    }

    for (0..message_length) |i| {
        count[i] = std.AutoHashMap(u8, usize).init(allocator);
    }

    for (messages) |message| {
        for (0..message_length) |j| {
            const char = message[j];
            const current_count = count[j].get(char) orelse 0;
            try count[j].put(char, current_count + 1);
        }
    }

    var original_message = try allocator.alloc(u8, message_length);
    for (0..message_length) |i| {
        original_message[i] = getLeastCommonChar(count[i]);
    }

    return original_message;
}

fn getLeastCommonChar(char_count: std.AutoHashMap(u8, usize)) u8 {
    var min_char: u8 = undefined;
    var min_count: usize = std.math.maxInt(usize);

    var iterator = char_count.iterator();
    while (iterator.next()) |entry| {
        if (entry.value_ptr.* < min_count) {
            min_count = entry.value_ptr.*;
            min_char = entry.key_ptr.*;
        }
    }

    return min_char;
}
