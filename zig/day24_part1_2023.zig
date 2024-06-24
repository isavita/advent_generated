
const std = @import("std");

const Coord = struct {
    x: f64,
    y: f64,
    z: f64,
};

const Point = struct {
    pos: Coord,
    vel: Coord,
};

fn parseInput(allocator: std.mem.Allocator, input: []const u8) ![]Point {
    var points = std.ArrayList(Point).init(allocator);
    defer points.deinit();

    var lines = std.mem.split(u8, input, "\n");
    while (lines.next()) |line| {
        var point: Point = undefined;
        var parts = std.mem.split(u8, line, "@");
        var pos_parts = std.mem.split(u8, parts.next().?, ",");
        var vel_parts = std.mem.split(u8, parts.next().?, ",");

        point.pos.x = try std.fmt.parseFloat(f64, std.mem.trim(u8, pos_parts.next().?, " "));
        point.pos.y = try std.fmt.parseFloat(f64, std.mem.trim(u8, pos_parts.next().?, " "));
        point.pos.z = try std.fmt.parseFloat(f64, std.mem.trim(u8, pos_parts.next().?, " "));
        point.vel.x = try std.fmt.parseFloat(f64, std.mem.trim(u8, vel_parts.next().?, " "));
        point.vel.y = try std.fmt.parseFloat(f64, std.mem.trim(u8, vel_parts.next().?, " "));
        point.vel.z = try std.fmt.parseFloat(f64, std.mem.trim(u8, vel_parts.next().?, " "));

        try points.append(point);
    }

    return points.toOwnedSlice();
}

fn isIntersecting2D(p1: Point, p2: Point, min: f64, max: f64) bool {
    const det = p1.vel.x * p2.vel.y - p2.vel.x * p1.vel.y;
    if (det == 0) return false;

    const t1 = (p2.vel.y * (p2.pos.x - p1.pos.x) - p2.vel.x * (p2.pos.y - p1.pos.y)) / det;
    const t2 = (p1.vel.y * (p2.pos.x - p1.pos.x) - p1.vel.x * (p2.pos.y - p1.pos.y)) / det;

    if (t1 < 0 or t2 < 0) return false;

    const x = p1.pos.x + p1.vel.x * t1;
    const y = p1.pos.y + p1.vel.y * t1;

    return x >= min and x <= max and y >= min and y <= max;
}

fn solve(points: []const Point, min: f64, max: f64) usize {
    var cnt: usize = 0;
    for (points[0..points.len-1], 0..) |p1, i| {
        for (points[i+1..]) |p2| {
            if (isIntersecting2D(p1, p2, min, max)) {
                cnt += 1;
            }
        }
    }
    return cnt;
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const input = try std.fs.cwd().readFileAlloc(allocator, "input.txt", 1024 * 1024);
    defer allocator.free(input);

    const points = try parseInput(allocator, input);
    defer allocator.free(points);

    const result = solve(points, 200000000000000, 400000000000000);
    try std.io.getStdOut().writer().print("{}\n", .{result});
}
