const std = @import("std");

pub fn main() !void {
    var file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var reader = std.io.bufferedReader(file.reader());
    var line_reader = reader.reader();

    var twos_count: i32 = 0;
    var threes_count: i32 = 0;

    var buffer: [1024]u8 = undefined;
    while (try line_reader.readUntilDelimiterOrEof(&buffer, '\n')) |line| {
        var letter_counts = [_]i32{0} ** 26;

        for (line) |char| {
            const idx = char - 'a';
            if (idx >= 0 and idx < 26) {
                letter_counts[idx] += 1;
            }
        }

        var found_twos: bool = false;
        var found_threes: bool = false;

        for (letter_counts) |count| {
            if (count == 2) found_twos = true;
            if (count == 3) found_threes = true;
        }

        if (found_twos) twos_count += 1;
        if (found_threes) threes_count += 1;
    }

    const checksum = twos_count * threes_count;
    std.debug.print("{}\n", .{checksum});
}