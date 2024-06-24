
const std = @import("std");

const Point = struct {
    x: i32,
    y: i32,
};

const Neighbors4 = [_]Point{
    .{ .x = 0, .y = 1 },
    .{ .x = 0, .y = -1 },
    .{ .x = 1, .y = 0 },
    .{ .x = -1, .y = 0 },
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var grid = std.AutoHashMap(Point, u8).init(allocator);
    defer grid.deinit();

    var visible = std.AutoHashMap(Point, void).init(allocator);
    defer visible.deinit();

    var buf: [1024]u8 = undefined;
    var y: i32 = 0;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        for (line, 0..) |b, x| {
            try grid.put(.{ .x = @intCast(x), .y = y }, b - '0');
        }
        y += 1;
    }

    var it = grid.iterator();
    while (it.next()) |entry| {
        const p = entry.key_ptr.*;
        const height = entry.value_ptr.*;

        outer: for (Neighbors4) |n| {
            var next = p;
            while (true) {
                next.x += n.x;
                next.y += n.y;
                if (grid.get(next)) |neighbor_height| {
                    if (neighbor_height >= height) {
                        continue :outer;
                    }
                } else {
                    try visible.put(p, {});
                    break :outer;
                }
            }
        }
    }

    const stdout = std.io.getStdOut().writer();
    try stdout.print("{d}\n", .{visible.count()});
}
