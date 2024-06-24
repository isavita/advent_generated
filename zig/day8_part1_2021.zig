
const std = @import("std");

pub fn main() !void {
    var file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var buf: [1024]u8 = undefined;
    var count: usize = 0;

    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var it = std.mem.split(u8, line, " | ");
        _ = it.next();
        if (it.next()) |output| {
            var digit_it = std.mem.split(u8, output, " ");
            while (digit_it.next()) |digit| {
                switch (digit.len) {
                    2, 3, 4, 7 => count += 1,
                    else => {},
                }
            }
        }
    }

    const stdout = std.io.getStdOut().writer();
    try stdout.print("{d}\n", .{count});
}
