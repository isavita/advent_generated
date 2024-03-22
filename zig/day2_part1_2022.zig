const std = @import("std");

pub fn main() !void {
    var file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var reader = std.io.bufferedReader(file.reader());
    var line_reader = reader.reader();

    var total_score: i32 = 0;

    var buffer: [1024]u8 = undefined;
    while (try line_reader.readUntilDelimiterOrEof(&buffer, '\n')) |line| {
        const opponent = line[0];
        const your_move = line[2];

        var score: i32 = 0;
        if (your_move == 'X') {
            score = 1;
        } else if (your_move == 'Y') {
            score = 2;
        } else if (your_move == 'Z') {
            score = 3;
        }

        if ((opponent == 'A' and your_move == 'Y') or (opponent == 'B' and your_move == 'Z') or (opponent == 'C' and your_move == 'X')) {
            score += 6;
        } else if ((opponent == 'A' and your_move == 'X') or (opponent == 'B' and your_move == 'Y') or (opponent == 'C' and your_move == 'Z')) {
            score += 3;
        }

        total_score += score;
    }

    std.debug.print("{}\n", .{total_score});
}