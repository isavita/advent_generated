
const std = @import("std");

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var lines = std.ArrayList([]const u8).init(allocator);
    defer lines.deinit();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();
    var buf: [1024]u8 = undefined;

    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        try lines.append(try allocator.dupe(u8, line));
    }

    const stdout = std.io.getStdOut().writer();

    outer: for (lines.items, 0..) |line1, i| {
        for (lines.items[i + 1..]) |line2| {
            var diff: u8 = 0;
            var common = std.ArrayList(u8).init(allocator);

            for (line1, line2) |c1, c2| {
                if (c1 != c2) {
                    diff += 1;
                    if (diff > 1) break;
                } else {
                    try common.append(c1);
                }
            }

            if (diff == 1) {
                try stdout.print("{s}\n", .{common.items});
                break :outer;
            }
        }
    }
}
