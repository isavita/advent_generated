const std = @import("std");

pub fn main() !void {
    var file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var reader = std.io.bufferedReader(file.reader());
    var buf_reader = reader.reader();

    var depths = std.ArrayList(i32).init(std.heap.page_allocator);
    defer depths.deinit();

    var buf: [1024]u8 = undefined;
    while (try buf_reader.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        const depth = try std.fmt.parseInt(i32, line, 10);
        try depths.append(depth);
    }

    var count: i32 = 0;
    for (0..depths.items.len - 3) |i| {
        const sum1 = depths.items[i] + depths.items[i + 1] + depths.items[i + 2];
        const sum2 = depths.items[i + 1] + depths.items[i + 2] + depths.items[i + 3];
        if (sum2 > sum1) {
            count += 1;
        }
    }

    std.debug.print("{}\n", .{count});
}