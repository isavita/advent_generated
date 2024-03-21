const std = @import("std");

pub fn main() !void {
    var file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var reader = std.io.bufferedReader(file.reader());
    var buffer: [1024]u8 = undefined;
    var line_reader = reader.reader();

    var horizontal_position: i32 = 0;
    var depth: i32 = 0;

    while (try line_reader.readUntilDelimiterOrEof(&buffer, '\n')) |line| {
        var parts = std.mem.split(u8, line, " ");
        const direction = parts.next().?;
        const units = try std.fmt.parseInt(i32, parts.next().?, 10);

        if (std.mem.eql(u8, direction, "forward")) {
            horizontal_position += units;
        } else if (std.mem.eql(u8, direction, "down")) {
            depth += units;
        } else if (std.mem.eql(u8, direction, "up")) {
            depth -= units;
        }
    }

    const product = horizontal_position * depth;
    std.debug.print("{}\n", .{product});
}