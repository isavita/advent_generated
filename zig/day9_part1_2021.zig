
const std = @import("std");

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var heightmap = std.ArrayList([]u8).init(allocator);
    defer {
        for (heightmap.items) |row| {
            allocator.free(row);
        }
        heightmap.deinit();
    }

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();
    var buf: [1024]u8 = undefined;

    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var row = try allocator.alloc(u8, line.len);
        for (line, 0..) |char, i| {
            row[i] = char - '0';
        }
        try heightmap.append(row);
    }

    var total_risk_level: u32 = 0;
    const rows = heightmap.items.len;
    const cols = heightmap.items[0].len;

    for (heightmap.items, 0..) |row, y| {
        for (row, 0..) |height, x| {
            if (isLowPoint(heightmap.items, x, y, rows, cols)) {
                total_risk_level += 1 + height;
            }
        }
    }

    try std.io.getStdOut().writer().print("{d}\n", .{total_risk_level});
}

fn isLowPoint(heightmap: []const []const u8, x: usize, y: usize, rows: usize, cols: usize) bool {
    const height = heightmap[y][x];
    if (x > 0 and heightmap[y][x - 1] <= height) return false;
    if (x < cols - 1 and heightmap[y][x + 1] <= height) return false;
    if (y > 0 and heightmap[y - 1][x] <= height) return false;
    if (y < rows - 1 and heightmap[y + 1][x] <= height) return false;
    return true;
}
