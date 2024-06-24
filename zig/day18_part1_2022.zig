
const std = @import("std");

const Point = struct {
    x: i32,
    y: i32,
    z: i32,
};

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var cubes = std.AutoHashMap(Point, void).init(allocator);
    defer cubes.deinit();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var buf: [64]u8 = undefined;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var it = std.mem.split(u8, line, ",");
        const x = try std.fmt.parseInt(i32, it.next().?, 10);
        const y = try std.fmt.parseInt(i32, it.next().?, 10);
        const z = try std.fmt.parseInt(i32, it.next().?, 10);
        try cubes.put(Point{ .x = x, .y = y, .z = z }, {});
    }

    var surface_area: u32 = 0;
    var it = cubes.keyIterator();
    while (it.next()) |cube| {
        surface_area += calculateExposedSides(cube.*, cubes);
    }

    const stdout = std.io.getStdOut().writer();
    try stdout.print("{}\n", .{surface_area});
}

fn calculateExposedSides(p: Point, cubes: std.AutoHashMap(Point, void)) u32 {
    const directions = [_]Point{
        .{ .x = 1, .y = 0, .z = 0 },  .{ .x = -1, .y = 0, .z = 0 },
        .{ .x = 0, .y = 1, .z = 0 },  .{ .x = 0, .y = -1, .z = 0 },
        .{ .x = 0, .y = 0, .z = 1 },  .{ .x = 0, .y = 0, .z = -1 },
    };

    var exposed_sides: u32 = 6;
    for (directions) |dir| {
        const adjacent = Point{
            .x = p.x + dir.x,
            .y = p.y + dir.y,
            .z = p.z + dir.z,
        };
        if (cubes.contains(adjacent)) {
            exposed_sides -= 1;
        }
    }
    return exposed_sides;
}
