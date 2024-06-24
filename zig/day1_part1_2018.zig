
const std = @import("std");

pub fn main() !void {
    // Open the input file
    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    // Create a buffered reader
    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var frequency: i32 = 0;
    var buf: [20]u8 = undefined;

    // Read the file line by line
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        // Parse the integer from the line
        const change = try std.fmt.parseInt(i32, line, 10);
        frequency += change;
    }

    // Print the final frequency
    const stdout = std.io.getStdOut().writer();
    try stdout.print("Final frequency: {}\n", .{frequency});
}
