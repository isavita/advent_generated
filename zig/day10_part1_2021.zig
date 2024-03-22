const std = @import("std");

fn checkLine(line: []const u8) ?u64 {
    const pairings = [_]u8{ '(', ')', '[', ']', '{', '}', '<', '>' };
    const scores = [_]u64{ 3, 3, 57, 57, 1197, 1197, 25137, 25137 };
    var stack: [128]u8 = undefined;
    var top: usize = 0;

    for (line) |char| {
        const index = std.mem.indexOfScalar(u8, &pairings, char).?;
        switch (index) {
            0, 2, 4, 6 => { // opening characters
                stack[top] = char;
                top += 1;
            },
            1, 3, 5, 7 => { // closing characters
                if (top == 0 or stack[top - 1] != pairings[index - 1]) {
                    return scores[index]; // corrupted line
                }
                top -= 1;
            },
            else => unreachable,
        }
    }

    return null; // line is not corrupted
}

pub fn main() !void {
    var file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var reader = std.io.bufferedReader(file.reader());
    var buf_reader = reader.reader();
    var buf: [1024]u8 = undefined;

    var total_score: u64 = 0;
    while (try buf_reader.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        if (checkLine(line)) |score| {
            total_score += score;
        }
    }

    std.debug.print("{}\n", .{total_score});
}