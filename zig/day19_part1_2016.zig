const std = @import("std");

pub fn main() !void {
    const total_elves = try readInput("input.txt");
    const winner = findWinningElf(total_elves);
    std.debug.print("{}\n", .{winner});
}

fn readInput(filename: []const u8) !usize {
    var file = try std.fs.cwd().openFile(filename, .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();
    var buf: [100]u8 = undefined;
    const line = try in_stream.readUntilDelimiterOrEof(&buf, '\n');
    if (line) |l| {
        return try std.fmt.parseInt(usize, l, 10);
    } else {
        return error.FailedToReadInput;
    }
}

fn findWinningElf(total_elves: usize) usize {
    var highest_power_of_two: usize = 1;
    while (highest_power_of_two * 2 <= total_elves) {
        highest_power_of_two *= 2;
    }
    return (total_elves - highest_power_of_two) * 2 + 1;
}