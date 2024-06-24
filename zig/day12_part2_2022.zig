
const std = @import("std");
const ArrayList = std.ArrayList;
const AutoHashMap = std.AutoHashMap;
const PriorityQueue = std.PriorityQueue;

const Point = struct {
    x: i32,
    y: i32,
};

const Item = struct {
    point: Point,
    priority: i32,
};

fn lessThan(context: void, a: Item, b: Item) std.math.Order {
    _ = context;
    return std.math.order(b.priority, a.priority);
}

const neighbors = [_]Point{ .{ .x = 0, .y = 1 }, .{ .x = 0, .y = -1 }, .{ .x = 1, .y = 0 }, .{ .x = -1, .y = 0 } };

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var grid = AutoHashMap(Point, u8).init(allocator);
    defer grid.deinit();

    var file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var start: Point = undefined;
    var end: Point = undefined;
    var as = ArrayList(Point).init(allocator);
    defer as.deinit();

    var y: i32 = 0;
    var buf: [1024]u8 = undefined;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        for (line, 0..) |b, x| {
            const p = Point{ .x = @intCast(x), .y = y };
            try grid.put(p, b);
            switch (b) {
                'S' => start = p,
                'E' => end = p,
                'a' => try as.append(p),
                else => {},
            }
        }
        y += 1;
    }

    try grid.put(start, 'a');
    try grid.put(end, 'z');

    var dists = try dijkstra(allocator, &grid, end);
    defer dists.deinit();

    var l = dists.get(start) orelse std.math.maxInt(i32);

    for (as.items) |a| {
        if (dists.get(a)) |d| {
            l = @min(l, d);
        }
    }

    const stdout = std.io.getStdOut().writer();
    try stdout.print("{d}\n", .{l});
}

fn dijkstra(allocator: std.mem.Allocator, grid: *AutoHashMap(Point, u8), end: Point) !AutoHashMap(Point, i32) {
    var pq = PriorityQueue(Item, void, lessThan).init(allocator, {});
    defer pq.deinit();

    var dist = AutoHashMap(Point, i32).init(allocator);
    try dist.put(end, 0);

    try pq.add(.{ .point = end, .priority = 0 });

    while (pq.removeOrNull()) |curr| {
        for (neighbors) |n| {
            const next = Point{ .x = curr.point.x + n.x, .y = curr.point.y + n.y };
            if (grid.get(next) == null) continue;

            const curr_height = grid.get(curr.point).?;
            const next_height = grid.get(next).?;
            if (@as(i32, curr_height) - @as(i32, next_height) > 1) continue;

            const next_dist = dist.get(curr.point).? + 1;
            const existing_dist = dist.get(next);

            if (existing_dist == null or next_dist < existing_dist.?) {
                try dist.put(next, next_dist);
                try pq.add(.{ .point = next, .priority = next_dist });
            }
        }
    }

    return dist;
}
