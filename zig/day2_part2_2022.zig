const std = @import("std");

pub fn main() !void {
    var file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var reader = std.io.bufferedReader(file.reader());
    var line_reader = reader.reader();

    var total_score: u32 = 0;

    var buffer: [1024]u8 = undefined;
    while (try line_reader.readUntilDelimiterOrEof(&buffer, '\n')) |line| {
        const opponent = line[0];
        const result = line[2];

        var score: u32 = 0;

        switch (result) {
            'X' => score = 0, // Need to lose
            'Y' => score = 3, // Need to draw
            'Z' => score = 6, // Need to win
            else => unreachable,
        }

        switch (opponent) {
            'A' => switch (result) {
                'X' => score += 3, // Rock vs Scissors
                'Y' => score += 1, // Rock vs Rock
                'Z' => score += 2, // Rock vs Paper
                else => unreachable,
            },
            'B' => switch (result) {
                'X' => score += 1, // Paper vs Rock
                'Y' => score += 2, // Paper vs Paper
                'Z' => score += 3, // Paper vs Scissors
                else => unreachable,
            },
            'C' => switch (result) {
                'X' => score += 2, // Scissors vs Paper
                'Y' => score += 3, // Scissors vs Scissors
                'Z' => score += 1, // Scissors vs Rock
                else => unreachable,
            },
            else => unreachable,
        }

        total_score += score;
    }

    std.debug.print("{}\n", .{total_score});
}