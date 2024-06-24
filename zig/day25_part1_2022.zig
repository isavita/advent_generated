
const std = @import("std");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var sum: i64 = 0;
    var buf: [64]u8 = undefined;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        sum += fromSnafu(line);
    }

    const result = try toSnafu(allocator, sum);
    defer allocator.free(result);

    try std.io.getStdOut().writer().print("{s}\n", .{result});
}

fn fromSnafu(s: []const u8) i64 {
    var n: i64 = 0;
    for (s) |c| {
        n *= 5;
        switch (c) {
            '=' => n -= 2,
            '-' => n -= 1,
            else => n += @as(i64, c - '0'),
        }
    }
    return n;
}

fn toSnafu(allocator: std.mem.Allocator, n: i64) ![]u8 {
    var list = std.ArrayList(u8).init(allocator);
    errdefer list.deinit();

    var num = n;
    while (num > 0) {
        const rem = @mod(num, 5);
        num = @divFloor(num, 5);
        switch (rem) {
            0 => try list.append('0'),
            1 => try list.append('1'),
            2 => try list.append('2'),
            3 => {
                try list.append('=');
                num += 1;
            },
            4 => {
                try list.append('-');
                num += 1;
            },
            else => unreachable,
        }
    }

    std.mem.reverse(u8, list.items);
    return list.toOwnedSlice();
}
