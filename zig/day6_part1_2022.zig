const std = @import("std");

pub fn main() !void {
    var file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var buffer: [4]u8 = undefined;
    var index: usize = 0;

    while (true) {
        const char = in_stream.readByte() catch |err| switch (err) {
            error.EndOfStream => break,
            else => return err,
        };

        buffer[index % 4] = char;
        index += 1;

        if (index >= 4 and allDifferent(buffer)) {
            std.debug.print("{}\n", .{index});
            return;
        }
    }
}

fn allDifferent(buffer: [4]u8) bool {
    return buffer[0] != buffer[1] and buffer[0] != buffer[2] and buffer[0] != buffer[3] and
           buffer[1] != buffer[2] and buffer[1] != buffer[3] and
           buffer[2] != buffer[3];
}