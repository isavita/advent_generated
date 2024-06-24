
const std = @import("std");

fn calculateWaysToWinLongRace(time: u64, record: u64) u64 {
    var low: u64 = 1;
    var high: u64 = time / 2;
    var waysToWin: u64 = 0;

    while (low <= high) {
        const mid = (low + high) / 2;
        const distance = mid * (time - mid);

        if (distance > record) {
            waysToWin = time - 2 * mid + 1;
            high = mid - 1;
        } else {
            low = mid + 1;
        }
    }

    return waysToWin;
}

pub fn main() !void {
    var file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var buf: [1024]u8 = undefined;
    var time: u64 = 0;
    var distance: u64 = 0;

    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        if (line.len == 0) continue;
        var parts = std.mem.split(u8, line, ":");
        _ = parts.next();
        const value = parts.next().?;
        var num_str = std.mem.trim(u8, value, " ");
        var num: u64 = 0;
        for (num_str) |c| {
            if (c >= '0' and c <= '9') {
                num = num * 10 + (c - '0');
            }
        }
        if (time == 0) {
            time = num;
        } else {
            distance = num;
        }
    }

    const waysToWin = calculateWaysToWinLongRace(time, distance);

    const stdout = std.io.getStdOut().writer();
    try stdout.print("{d}\n", .{waysToWin});
}
