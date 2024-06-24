
const std = @import("std");

pub fn main() !void {
    var file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var buf: [1024]u8 = undefined;
    var sum: u32 = 0;

    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var first_digit: u8 = 0;
        var last_digit: u8 = 0;

        for (line) |char| {
            if (std.ascii.isDigit(char)) {
                if (first_digit == 0) {
                    first_digit = char - '0';
                }
                last_digit = char - '0';
            }
        }

        if (first_digit != 0) {
            sum += first_digit * 10 + last_digit;
        }
    }

    try std.io.getStdOut().writer().print("{d}\n", .{sum});
}
