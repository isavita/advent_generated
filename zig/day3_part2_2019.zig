const std = @import("std");

const Point = struct {
    x: i32,
    y: i32,
};

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(content);

    var lines = std.mem.split(u8, content, "\n");
    const wire1 = lines.next().?;
    const wire2 = lines.next().?;

    var wire1_points = std.AutoHashMap(Point, i32).init(allocator);
    defer wire1_points.deinit();

    var x: i32 = 0;
    var y: i32 = 0;
    var steps: i32 = 0;

    var directions = std.mem.split(u8, wire1, ",");
    while (directions.next()) |dir| {
        const d = dir[0];
        const len = try std.fmt.parseInt(i32, dir[1..], 10);

        var i: i32 = 0;
        while (i < len) : (i += 1) {
            steps += 1;
            switch (d) {
                'R' => x += 1,
                'L' => x -= 1,
                'U' => y += 1,
                'D' => y -= 1,
                else => unreachable,
            }
            _ = try wire1_points.put(Point{ .x = x, .y = y }, steps);
        }
    }

    x = 0;
    y = 0;
    steps = 0;
    var min_steps: ?i32 = null;

    directions = std.mem.split(u8, wire2, ",");
    while (directions.next()) |dir| {
        const d = dir[0];
        const len = try std.fmt.parseInt(i32, dir[1..], 10);

        var i: i32 = 0;
        while (i < len) : (i += 1) {
            steps += 1;
            switch (d) {
                'R' => x += 1,
                'L' => x -= 1,
                'U' => y += 1,
                'D' => y -= 1,
                else => unreachable,
            }

            if (wire1_points.get(Point{ .x = x, .y = y })) |p_steps| {
                const total_steps = p_steps + steps;
                if (min_steps == null or total_steps < min_steps.?) {
                    min_steps = total_steps;
                }
            }
        }
    }

    if (min_steps) |result| {
        try std.io.getStdOut().writer().print("{d}\n", .{result});
    } else {
        try std.io.getStdOut().writer().print("No intersection found\n", .{});
    }
}