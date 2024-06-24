
const std = @import("std");
const fs = std.fs;
const io = std.io;
const mem = std.mem;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file = try fs.cwd().openFile("input.txt", .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(content);

    const trimmed = mem.trim(u8, content, &std.ascii.whitespace);

    const part1_sum = try solvePart1(trimmed);
    const part2_sum = try solvePart2(trimmed);

    try std.io.getStdOut().writer().print("Part 1: {}\n", .{part1_sum});
    try std.io.getStdOut().writer().print("Part 2: {}\n", .{part2_sum});
}

fn solvePart1(input: []const u8) !u32 {
    var sum: u32 = 0;
    const len = input.len;

    for (input, 0..) |digit, i| {
        const next_digit = input[(i + 1) % len];
        if (digit == next_digit) {
            sum += try std.fmt.charToDigit(digit, 10);
        }
    }

    return sum;
}

fn solvePart2(input: []const u8) !u32 {
    var sum: u32 = 0;
    const len = input.len;
    const half_len = len / 2;

    for (input, 0..) |digit, i| {
        const halfway_digit = input[(i + half_len) % len];
        if (digit == halfway_digit) {
            sum += try std.fmt.charToDigit(digit, 10);
        }
    }

    return sum;
}
