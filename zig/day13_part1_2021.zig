
const std = @import("std");

const Point = struct {
    x: u16,
    y: u16,
};

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var points = std.AutoHashMap(Point, void).init(allocator);
    defer points.deinit();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var buf: [64]u8 = undefined;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        if (line.len == 0) break;
        var it = std.mem.split(u8, line, ",");
        const x = try std.fmt.parseUnsigned(u16, it.next().?, 10);
        const y = try std.fmt.parseUnsigned(u16, it.next().?, 10);
        try points.put(Point{ .x = x, .y = y }, {});
    }

    if (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |fold_line| {
        var it = std.mem.split(u8, fold_line, "=");
        _ = it.next();
        const value = try std.fmt.parseUnsigned(u16, it.next().?, 10);

        var new_points = std.AutoHashMap(Point, void).init(allocator);
        var it_points = points.keyIterator();
        while (it_points.next()) |point| {
            var new_point = point.*;
            if (fold_line[11] == 'x' and point.x > value) {
                new_point.x = 2 * value - point.x;
            } else if (fold_line[11] == 'y' and point.y > value) {
                new_point.y = 2 * value - point.y;
            }
            try new_points.put(new_point, {});
        }

        const stdout = std.io.getStdOut().writer();
        try stdout.print("{d}\n", .{new_points.count()});
    }
}
