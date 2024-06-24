
const std = @import("std");

const Point = struct {
    x: i32,
    y: i32,

    fn add(self: Point, other: Point) Point {
        return .{ .x = self.x + other.x, .y = self.y + other.y };
    }

    fn sub(self: Point, other: Point) Point {
        return .{ .x = self.x - other.x, .y = self.y - other.y };
    }
};

const Dir = enum(u8) {
    N,
    E,
    S,
    W,

    fn point(self: Dir) Point {
        return switch (self) {
            .N => .{ .x = 0, .y = 1 },
            .E => .{ .x = 1, .y = 0 },
            .S => .{ .x = 0, .y = -1 },
            .W => .{ .x = -1, .y = 0 },
        };
    }
};

fn dirFromByte(b: u8) Dir {
    return switch (b) {
        '>' => .E,
        '<' => .W,
        else => unreachable,
    };
}

const rocks = [_][]const Point{
    &[_]Point{ .{ .x = 0, .y = 0 }, .{ .x = 1, .y = 0 }, .{ .x = 2, .y = 0 }, .{ .x = 3, .y = 0 } },
    &[_]Point{ .{ .x = 1, .y = 0 }, .{ .x = 0, .y = 1 }, .{ .x = 1, .y = 1 }, .{ .x = 2, .y = 1 }, .{ .x = 1, .y = 2 } },
    &[_]Point{ .{ .x = 0, .y = 0 }, .{ .x = 1, .y = 0 }, .{ .x = 2, .y = 0 }, .{ .x = 2, .y = 1 }, .{ .x = 2, .y = 2 } },
    &[_]Point{ .{ .x = 0, .y = 0 }, .{ .x = 0, .y = 1 }, .{ .x = 0, .y = 2 }, .{ .x = 0, .y = 3 } },
    &[_]Point{ .{ .x = 0, .y = 0 }, .{ .x = 1, .y = 0 }, .{ .x = 0, .y = 1 }, .{ .x = 1, .y = 1 } },
};

fn collision(grid: *std.AutoHashMap(Point, void), rock: []const Point, pos: Point) bool {
    for (rock) |p| {
        const np = p.add(pos);
        if (np.x < 0 or np.x > 6 or grid.contains(np)) {
            return true;
        }
    }
    return false;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    const jet_pattern = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(jet_pattern);

    var grid = std.AutoHashMap(Point, void).init(allocator);
    defer grid.deinit();

    var x: i32 = 0;
    while (x < 7) : (x += 1) {
        try grid.put(.{ .x = x, .y = 0 }, {});
    }

    var floor: i32 = 0;
    var j: usize = 0;

    var i: usize = 0;
    while (i < 2022) : (i += 1) {
        const curr_rock = rocks[i % rocks.len];
        var pos = Point{ .x = 2, .y = floor + 4 };

        while (true) {
            const jet = jet_pattern[j];
            j = (j + 1) % jet_pattern.len;

            const jet_dir = dirFromByte(jet);
            pos = pos.add(jet_dir.point());
            if (collision(&grid, curr_rock, pos)) {
                pos = pos.sub(jet_dir.point());
            }

            pos = pos.add(Dir.S.point());
            if (collision(&grid, curr_rock, pos)) {
                pos = pos.sub(Dir.S.point());
                for (curr_rock) |p| {
                    const np = p.add(pos);
                    try grid.put(np, {});
                    if (np.y > floor) {
                        floor = np.y;
                    }
                }
                break;
            }
        }
    }

    const stdout = std.io.getStdOut().writer();
    try stdout.print("{}\n", .{floor});
}
