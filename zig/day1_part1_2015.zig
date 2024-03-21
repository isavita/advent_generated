const std = @import("std");

pub fn main() !void {
    var file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var floor: i32 = 0;

    while (true) {
        const char = in_stream.readByte() catch |err| switch (err) {
            error.EndOfStream => break,
            else => return err,
        };

        switch (char) {
            '(' => floor += 1,
            ')' => floor -= 1,
            else => unreachable,
        }
    }

    std.debug.print("Final floor: {}\n", .{floor});
}
