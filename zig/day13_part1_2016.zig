
const std = @import("std");

const Point = struct { x: i32, y: i32 };

fn isWall(favoriteNumber: i32, x: i32, y: i32) bool {
    const num = x * x + 3 * x + 2 * x * y + y + y * y + favoriteNumber;
    var bits: i32 = 0;
    var n = num;
    while (n > 0) : (n >>= 1) {
        if (n & 1 != 0) bits += 1;
    }
    return bits % 2 != 0;
}

fn bfs(start: Point, target: Point, favoriteNumber: i32) i32 {
    var visited = std.AutoHashMap(Point, void).init(std.heap.page_allocator);
    defer visited.deinit();
    var queue = std.ArrayList(Point).init(std.heap.page_allocator);
    defer queue.deinit();
    try queue.append(start);
    var steps: i32 = 0;

    while (queue.items.len > 0) : (steps += 1) {
        const size = queue.items.len;
        for (queue.items[0..size]) |point| {
            if (point.x == target.x and point.y == target.y) return steps;
            const deltas = [4]Point{.{1, 0}, .{-1, 0}, .{0, 1}, .{0, -1}};
            for (deltas) |delta| {
                const next = Point{ .x = point.x + delta.x, .y = point.y + delta.y };
                if (next.x >= 0 and next.y >= 0 and !isWall(favoriteNumber, next.x, next.y)) {
                    if (!visited.contains(next)) {
                        try visited.put(next, {});
                        try queue.append(next);
                    }
                }
            }
        }
        queue.shrinkRetainingCapacity(size);
    }

    return -1;
}

pub fn main() !void {
    const file = try std.fs.cwd().openFile("input.txt", .{ .read = true });
    defer file.close();
    const num = try std.fmt.parseInt(i32, try file.reader().readUntilDelimiterAlloc(std.heap.page_allocator, '\n', 100), 10);
    const start = Point{ .x = 1, .y = 1 };
    const target = Point{ .x = 31, .y = 39 };
    const steps = bfs(start, target, num);
    std.debug.print("{}\n", .{steps});
}
