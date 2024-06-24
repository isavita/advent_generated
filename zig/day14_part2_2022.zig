
const std = @import("std");
const math = std.math;
const mem = std.mem;
const ArrayList = std.ArrayList;

const Point = struct { x: usize, y: usize };

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    const input = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(input);

    const result = try solve(allocator, input);
    try std.io.getStdOut().writer().print("{d}\n", .{result});
}

fn solve(allocator: mem.Allocator, input: []const u8) !usize {
    var rocks = ArrayList(Point).init(allocator);
    defer rocks.deinit();

    var it = mem.tokenize(u8, input, "\n");
    var max_y: usize = 0;
    while (it.next()) |line| {
        var coords = mem.tokenize(u8, line, " -> ");
        var prev: ?Point = null;
        while (coords.next()) |coord| {
            var parts = mem.split(u8, coord, ",");
            const x = try std.fmt.parseUnsigned(usize, parts.next().?, 10);
            const y = try std.fmt.parseUnsigned(usize, parts.next().?, 10);
            max_y = @max(max_y, y);
            if (prev) |p| {
                if (p.x == x) {
                    const start = @min(p.y, y);
                    const end = @max(p.y, y);
                    var i = start;
                    while (i <= end) : (i += 1) {
                        try rocks.append(.{ .x = x, .y = i });
                    }
                } else {
                    const start = @min(p.x, x);
                    const end = @max(p.x, x);
                    var i = start;
                    while (i <= end) : (i += 1) {
                        try rocks.append(.{ .x = i, .y = y });
                    }
                }
            }
            prev = .{ .x = x, .y = y };
        }
    }

    const floor_y = max_y + 2;
    var sand = std.AutoHashMap(Point, void).init(allocator);
    defer sand.deinit();

    const start = Point{ .x = 500, .y = 0 };
    var count: usize = 0;
    while (!sand.contains(start)) : (count += 1) {
        var current = start;
        while (true) {
            if (current.y + 1 == floor_y) {
                _ = try sand.put(current, {});
                break;
            }
            const down = Point{ .x = current.x, .y = current.y + 1 };
            const left = Point{ .x = current.x - 1, .y = current.y + 1 };
            const right = Point{ .x = current.x + 1, .y = current.y + 1 };
            if (!sand.contains(down) and !contains(rocks.items, down)) {
                current = down;
            } else if (!sand.contains(left) and !contains(rocks.items, left)) {
                current = left;
            } else if (!sand.contains(right) and !contains(rocks.items, right)) {
                current = right;
            } else {
                _ = try sand.put(current, {});
                break;
            }
        }
    }

    return count;
}

fn contains(slice: []const Point, item: Point) bool {
    for (slice) |point| {
        if (point.x == item.x and point.y == item.y) return true;
    }
    return false;
}
