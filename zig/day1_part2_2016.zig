const std = @import("std");

const Direction = enum {
    North,
    East,
    South,
    West,
};

const Point = struct {
    x: i32,
    y: i32,
};

fn turnLeft(direction: Direction) Direction {
    return switch (direction) {
        .North => .West,
        .West => .South,
        .South => .East,
        .East => .North,
    };
}

fn turnRight(direction: Direction) Direction {
    return switch (direction) {
        .North => .East,
        .East => .South,
        .South => .West,
        .West => .North,
    };
}

fn moveForward(point: Point, direction: Direction) Point {
    return switch (direction) {
        .North => .{ .x = point.x, .y = point.y + 1 },
        .East => .{ .x = point.x + 1, .y = point.y },
        .South => .{ .x = point.x, .y = point.y - 1 },
        .West => .{ .x = point.x - 1, .y = point.y },
    };
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(content);

    var visited = std.AutoHashMap(Point, void).init(allocator);
    defer visited.deinit();

    var current = Point{ .x = 0, .y = 0 };
    var direction = Direction.North;
    try visited.put(current, {});

    var instructions = std.mem.split(u8, std.mem.trim(u8, content, "\n"), ", ");
    while (instructions.next()) |instruction| {
        const turn = instruction[0];
        const steps = try std.fmt.parseInt(i32, instruction[1..], 10);

        direction = if (turn == 'L') turnLeft(direction) else turnRight(direction);

        var i: i32 = 0;
        while (i < steps) : (i += 1) {
            current = moveForward(current, direction);
            if (visited.contains(current)) {
                const distance = @abs(current.x) + @abs(current.y);
                try std.io.getStdOut().writer().print("{}\n", .{distance});
                return;
            }
            try visited.put(current, {});
        }
    }
}