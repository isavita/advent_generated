
const std = @import("std");
const fs = std.fs;
const io = std.io;

pub fn main() !void {
    // Open the input file
    var file = try fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buf_reader = io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    // Read the number of elves from the file
    var buf: [10]u8 = undefined;
    const line = try in_stream.readUntilDelimiterOrEof(&buf, '\n');
    const n = try std.fmt.parseInt(u32, line.?, 10);

    // Solve Part 1
    const part1_result = solvePart1(n);
    try std.io.getStdOut().writer().print("Part 1: {}\n", .{part1_result});

    // Solve Part 2
    const part2_result = solvePart2(n);
    try std.io.getStdOut().writer().print("Part 2: {}\n", .{part2_result});
}

fn solvePart1(n: u32) u32 {
    var highest_power_of_2: u32 = 1;
    while (highest_power_of_2 * 2 <= n) {
        highest_power_of_2 *= 2;
    }
    return 2 * (n - highest_power_of_2) + 1;
}

fn solvePart2(n: u32) u32 {
    var pow: u32 = 1;
    while (pow * 3 < n) {
        pow *= 3;
    }
    if (n == pow) return n;
    if (n <= pow * 2) return n - pow;
    return 2 * n - 3 * pow;
}
