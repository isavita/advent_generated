
const std = @import("std");

const Coord = struct {
    x: i64,
    y: i64,

    fn add(self: Coord, other: Coord) Coord {
        return .{ .x = self.x + other.x, .y = self.y + other.y };
    }

    fn multiplyByScalar(self: Coord, s: i64) Coord {
        return .{ .x = self.x * s, .y = self.y * s };
    }
};

const North = Coord{ .x = 0, .y = -1 };
const West = Coord{ .x = -1, .y = 0 };
const South = Coord{ .x = 0, .y = 1 };
const East = Coord{ .x = 1, .y = 0 };

fn abs(x: i64) i64 {
    return if (x < 0) -x else x;
}

fn parseInput(input: []const u8, allocator: std.mem.Allocator) !std.ArrayList(Coord) {
    var vertices = std.ArrayList(Coord).init(allocator);
    var current = Coord{ .x = 0, .y = 0 };
    try vertices.append(current);

    var lines = std.mem.split(u8, input, "\n");
    while (lines.next()) |line| {
        const color = line[line.len - 7 .. line.len - 1];
        const dir_input = color[5];
        const length = try std.fmt.parseInt(i64, color[0..5], 16);

        const dir = switch (dir_input) {
            '3' => North,
            '2' => West,
            '1' => South,
            '0' => East,
            else => unreachable,
        };

        current = current.add(dir.multiplyByScalar(length));
        try vertices.append(current);
    }

    return vertices;
}

fn shoelace(vertices: []const Coord) i64 {
    var area: i64 = 0;
    const n = vertices.len;

    for (vertices, 0..) |v, i| {
        const next = vertices[(i + 1) % n];
        area += v.x * next.y;
        area -= v.y * next.x;
    }

    return @divExact(abs(area), 2);
}

fn perimeter(vertices: []const Coord) i64 {
    var perim: i64 = 0;
    const n = vertices.len;

    for (vertices, 0..) |v, i| {
        const next = vertices[(i + 1) % n];
        perim += abs(v.x - next.x) + abs(v.y - next.y);
    }

    return perim;
}

fn calculatePolygonArea(vertices: []const Coord) i64 {
    return shoelace(vertices) + @divExact(perimeter(vertices), 2) + 1;
}

fn solve(input: []const u8, allocator: std.mem.Allocator) !i64 {
    var vertices = try parseInput(input, allocator);
    defer vertices.deinit();

    return calculatePolygonArea(vertices.items);
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const input = try std.fs.cwd().readFileAlloc(allocator, "input.txt", 1024 * 1024);
    defer allocator.free(input);

    const result = try solve(input, allocator);
    try std.io.getStdOut().writer().print("{d}\n", .{result});
}
