
const std = @import("std");

const Point = struct {
    x: i32,
    y: i32,
};

const Neighbors4 = [_]Point{ .{ .x = 0, .y = 1 }, .{ .x = 0, .y = -1 }, .{ .x = 1, .y = 0 }, .{ .x = -1, .y = 0 } };

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

    var buf: [1024]u8 = undefined;
    var y: i32 = 0;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        for (line, 0..) |c, x| {
            try grid.put(.{ .x = @intCast(x), .y = y }, c - '0');
        }
        y += 1;
    }

    var max_score: u32 = 0;
    var it = grid.iterator();
    while (it.next()) |entry| {
        const p = entry.key_ptr.*;
        const height = entry.value_ptr.*;

        var score: u32 = 1;
        for (Neighbors4) |n| {
            var next = Point{ .x = p.x + n.x, .y = p.y + n.y };
            var view: u32 = 0;
            while (grid.get(next)) |next_height| {
                view += 1;
                if (next_height >= height) break;
                next.x += n.x;
                next.y += n.y;
            }
            score *= view;
        }

        max_score = @max(max_score, score);
    }

    try std.io.getStdOut().writer().print("{d}\n", .{max_score});
}
