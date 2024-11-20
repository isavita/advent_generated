
const std = @import("std");
const ArrayList = std.ArrayList;
const AutoHashMap = std.AutoHashMap;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var heightmap = ArrayList([]u8).init(allocator);
    defer {
        for (heightmap.items) |row| allocator.free(row);
        heightmap.deinit();
    }

    var buf: [1024]u8 = undefined;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var row = try allocator.alloc(u8, line.len);
        for (line, 0..) |char, i| {
            row[i] = char - '0';
        }
        try heightmap.append(row);
    }

    var visited = AutoHashMap([2]u32, bool).init(allocator);
    defer visited.deinit();

    var basin_sizes = ArrayList(u32).init(allocator);
    defer basin_sizes.deinit();

    for (heightmap.items, 0..) |row, y| {
        for (row, 0..) |_, x| {
            if (isLowPoint(heightmap.items, x, y)) {
                const size = try exploreBasin(allocator, heightmap.items, x, y, &visited);
                try basin_sizes.append(size);
            }
        }
    }

    std.sort.heap(u32, basin_sizes.items, {}, comptime std.sort.desc(u32));

    const result = basin_sizes.items[0] * basin_sizes.items[1] * basin_sizes.items[2];
    const stdout = std.io.getStdOut().writer();
    try stdout.print("{}\n", .{result});
}

fn isLowPoint(heightmap: [][]u8, x: usize, y: usize) bool {
    const height = heightmap[y][x];
    const width = heightmap[0].len;
    const height_map_height = heightmap.len;

    if (x > 0 and heightmap[y][x-1] <= height) return false;
    if (x < width - 1 and heightmap[y][x+1] <= height) return false;
    if (y > 0 and heightmap[y-1][x] <= height) return false;
    if (y < height_map_height - 1 and heightmap[y+1][x] <= height) return false;

    return true;
}

fn exploreBasin(allocator: std.mem.Allocator, heightmap: [][]u8, x: usize, y: usize, visited: *AutoHashMap([2]u32, bool)) !u32 {
    const key = [_]u32{ @intCast(x), @intCast(y) };
    if (visited.contains(key) or heightmap[y][x] == 9) return 0;

    try visited.put(key, true);
    var size: u32 = 1;

    const directions = [_][2]i2{ .{0, -1}, .{-1, 0}, .{0, 1}, .{1, 0} };
    for (directions) |dir| {
        const new_x = @as(i32, @intCast(x)) + dir[0];
        const new_y = @as(i32, @intCast(y)) + dir[1];

        if (new_x >= 0 and new_x < heightmap[0].len and new_y >= 0 and new_y < heightmap.len) {
            size += try exploreBasin(allocator, heightmap, @intCast(new_x), @intCast(new_y), visited);
        }
    }

    return size;
}
