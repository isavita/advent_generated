const std = @import("std");

const Coord = struct {
    x: i32,
    y: i32,
    z: i32,
    w: i32,
};

fn countActiveNeighbors(grid: std.AutoHashMap(Coord, void), coord: Coord) u8 {
    var count: u8 = 0;
    var dx: i32 = -1;
    while (dx <= 1) : (dx += 1) {
        var dy: i32 = -1;
        while (dy <= 1) : (dy += 1) {
            var dz: i32 = -1;
            while (dz <= 1) : (dz += 1) {
                var dw: i32 = -1;
                while (dw <= 1) : (dw += 1) {
                    if (dx == 0 and dy == 0 and dz == 0 and dw == 0) continue;
                    if (grid.contains(Coord{ .x = coord.x + dx, .y = coord.y + dy, .z = coord.z + dz, .w = coord.w + dw })) {
                        count += 1;
                    }
                }
            }
        }
    }
    return count;
}

fn simulateCycle(allocator: std.mem.Allocator, grid: std.AutoHashMap(Coord, void)) !std.AutoHashMap(Coord, void) {
    var new_grid = std.AutoHashMap(Coord, void).init(allocator);

    var it = grid.iterator();
    while (it.next()) |entry| {
        const coord = entry.key_ptr.*;
        var dx: i32 = -1;
        while (dx <= 1) : (dx += 1) {
            var dy: i32 = -1;
            while (dy <= 1) : (dy += 1) {
                var dz: i32 = -1;
                while (dz <= 1) : (dz += 1) {
                    var dw: i32 = -1;
                    while (dw <= 1) : (dw += 1) {
                        const neighbor = Coord{ .x = coord.x + dx, .y = coord.y + dy, .z = coord.z + dz, .w = coord.w + dw };
                        const active_neighbors = countActiveNeighbors(grid, neighbor);
                        if (grid.contains(neighbor)) {
                            if (active_neighbors == 2 or active_neighbors == 3) {
                                try new_grid.put(neighbor, {});
                            }
                        } else if (active_neighbors == 3) {
                            try new_grid.put(neighbor, {});
                        }
                    }
                }
            }
        }
    }

    return new_grid;
}

pub fn main() !void {
    const allocator = std.heap.page_allocator;

    // Read input from file
    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(content);

    var grid = std.AutoHashMap(Coord, void).init(allocator);
    defer grid.deinit();

    var y: i32 = 0;
    var lines = std.mem.split(u8, content, "\n");
    while (lines.next()) |line| {
        if (line.len == 0) continue;
        for (line, 0..) |c, x| {
            if (c == '#') {
                try grid.put(Coord{ .x = @intCast(x), .y = y, .z = 0, .w = 0 }, {});
            }
        }
        y += 1;
    }

    var cycle: u8 = 0;
    while (cycle < 6) : (cycle += 1) {
        grid = try simulateCycle(allocator, grid);
    }

    const active_count = grid.count();
    try std.io.getStdOut().writer().print("{d}\n", .{active_count});
}