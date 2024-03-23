const std = @import("std");

pub fn main() !void {
    var file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var reader = std.io.bufferedReader(file.reader());
    var line_reader = reader.reader();

    var horizontal_position: i32 = 0;
    var depth: i32 = 0;
    var aim: i32 = 0;

    var buffer: [1024]u8 = undefined;
    while (try line_reader.readUntilDelimiterOrEof(&buffer, '\n')) |line| {
        var parts = std.mem.split(u8, line, " ");
        const direction = parts.next().?;
        const units = try std.fmt.parseInt(i32, parts.next().?, 10);

        if (std.mem.eql(u8, direction, "forward")) {
            horizontal_position += units;
            depth += aim * units;
        } else if (std.mem.eql(u8, direction, "down")) {
            aim += units;
        } else if (std.mem.eql(u8, direction, "up")) {
            aim -= units;
        }
    }

    const product = horizontal_position * depth;
    std.debug.print("{d}\n", .{product});
}