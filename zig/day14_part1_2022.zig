
const std = @import("std");
const Point = struct { x: i32, y: i32 };

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var grid = std.AutoHashMap(Point, void).init(allocator);
    defer grid.deinit();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var buf: [1024]u8 = undefined;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var it = std.mem.split(u8, line, " -> ");
        var prev: ?Point = null;
        while (it.next()) |coord| {
            var pt_it = std.mem.split(u8, coord, ",");
            const x = try std.fmt.parseInt(i32, pt_it.next().?, 10);
            const y = try std.fmt.parseInt(i32, pt_it.next().?, 10);
            const curr = Point{ .x = x, .y = y };

            if (prev) |p| {
                var x1 = @min(p.x, curr.x);
                var x2 = @max(p.x, curr.x);
                var y1 = @min(p.y, curr.y);
                var y2 = @max(p.y, curr.y);

                while (x1 <= x2 and y1 <= y2) : ({
                    x1 += if (p.x == curr.x) 0 else 1;
                    y1 += if (p.y == curr.y) 0 else 1;
                }) {
                    try grid.put(Point{ .x = x1, .y = y1 }, {});
                }
            }
            prev = curr;
        }
    }

    const result = try fill(&grid);
    try std.io.getStdOut().writer().print("{}\n", .{result});
}

fn fill(grid: *std.AutoHashMap(Point, void)) !u32 {
    var floor: i32 = 0;
    var it = grid.iterator();
    while (it.next()) |entry| {
        floor = @max(floor, entry.key_ptr.y);
    }
    floor += 1;

    var sands: u32 = 0;
    var first_floor_touch: u32 = 0;
    while (!grid.contains(Point{ .x = 500, .y = 0 })) : (sands += 1) {
        var sand = Point{ .x = 500, .y = 0 };
        while (true) {
            if (sand.y == floor) {
                if (first_floor_touch == 0) {
                    first_floor_touch = sands;
                }
                try grid.put(sand, {});
                break;
            }
            const down = Point{ .x = sand.x, .y = sand.y + 1 };
            const left = Point{ .x = sand.x - 1, .y = sand.y + 1 };
            const right = Point{ .x = sand.x + 1, .y = sand.y + 1 };
            if (!grid.contains(down)) {
                sand = down;
            } else if (!grid.contains(left)) {
                sand = left;
            } else if (!grid.contains(right)) {
                sand = right;
            } else {
                try grid.put(sand, {});
                break;
            }
        }
    }
    return first_floor_touch;
}
