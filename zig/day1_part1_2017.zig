const std = @import("std");

pub fn main() !void {
    var file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var sum: u32 = 0;
    var prev_char: u8 = 0;
    var first_char: u8 = 0;

    while (true) {
        const char = in_stream.readByte() catch |err| switch (err) {
            error.EndOfStream => break,
            else => return err,
        };

        if (first_char == 0) {
            first_char = char;
        }

        if (prev_char != 0 and char == prev_char) {
            sum += @as(u32, char - '0');
        }

        prev_char = char;
    }

    if (prev_char == first_char) {
        sum += @as(u32, prev_char - '0');
    }

    std.log.info("{}", .{sum});
}