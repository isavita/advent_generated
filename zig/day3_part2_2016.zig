
const std = @import("std");
const fs = std.fs;
const io = std.io;
const mem = std.mem;

fn isValidTriangle(a: u32, b: u32, c: u32) bool {
    return a + b > c and b + c > a and c + a > b;
}

pub fn main() !void {
    var file = try fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buf_reader = io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var valid_count_part1: u32 = 0;
    var valid_count_part2: u32 = 0;
    var buffer: [64]u8 = undefined;
    var triangle_data: [3][3]u32 = undefined;
    var row: usize = 0;

    while (try in_stream.readUntilDelimiterOrEof(&buffer, '\n')) |line| {
        var it = mem.tokenize(u8, line, " ");
        var col: usize = 0;
        while (it.next()) |num_str| {
            if (num_str.len == 0) continue;
            triangle_data[row][col] = try std.fmt.parseInt(u32, num_str, 10);
            col += 1;
        }

        // Part 1
        if (isValidTriangle(triangle_data[row][0], triangle_data[row][1], triangle_data[row][2])) {
            valid_count_part1 += 1;
        }

        row += 1;
        if (row == 3) {
            // Part 2
            for (0..3) |i| {
                if (isValidTriangle(triangle_data[0][i], triangle_data[1][i], triangle_data[2][i])) {
                    valid_count_part2 += 1;
                }
            }
            row = 0;
        }
    }

    const stdout = io.getStdOut().writer();
    try stdout.print("Part 1: {}\n", .{valid_count_part1});
    try stdout.print("Part 2: {}\n", .{valid_count_part2});
}
