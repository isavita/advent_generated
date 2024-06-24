
const std = @import("std");

pub fn main() !void {
    var file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var buf: [1024]u8 = undefined;
    var total_points: u32 = 0;

    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var it = std.mem.split(u8, line, " | ");
        const winning_numbers = it.next().?;
        const your_numbers = it.next().?;
        total_points += calculatePoints(winning_numbers, your_numbers);
    }

    const stdout = std.io.getStdOut().writer();
    try stdout.print("{d}\n", .{total_points});
}

fn calculatePoints(winning_numbers: []const u8, your_numbers: []const u8) u32 {
    var points: u32 = 0;
    var winning_set = std.bit_set.IntegerBitSet(100).initEmpty();

    var it = std.mem.tokenize(u8, winning_numbers, " ");
    while (it.next()) |num_str| {
        const num = std.fmt.parseInt(u8, num_str, 10) catch continue;
        winning_set.set(num);
    }

    it = std.mem.tokenize(u8, your_numbers, " ");
    while (it.next()) |num_str| {
        const num = std.fmt.parseInt(u8, num_str, 10) catch continue;
        if (winning_set.isSet(num)) {
            points = if (points == 0) 1 else points * 2;
        }
    }

    return points;
}
