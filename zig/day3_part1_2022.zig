
const std = @import("std");

fn itemPriority(item: u8) u8 {
    return if (item >= 'a') item - 'a' + 1 else item - 'A' + 27;
}

pub fn main() !void {
    var file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var sum: u32 = 0;
    var buffer: [64]u8 = undefined;

    while (try in_stream.readUntilDelimiterOrEof(&buffer, '\n')) |line| {
        const half = line.len / 2;
        var seen = [_]bool{false} ** 256;

        for (line[0..half]) |item| {
            seen[item] = true;
        }

        for (line[half..]) |item| {
            if (seen[item]) {
                sum += itemPriority(item);
                break;
            }
        }
    }

    const stdout = std.io.getStdOut().writer();
    try stdout.print("{d}\n", .{sum});
}
