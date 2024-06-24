
const std = @import("std");
const math = std.math;
const Allocator = std.mem.Allocator;

const Position = struct {
    x: u16,
    y: u16,
    risk: u32,
};

fn lessThan(context: void, a: Position, b: Position) std.math.Order {
    _ = context;
    return std.math.order(a.risk, b.risk);
}

fn dijkstra(allocator: Allocator, grid: []const []const u8) !u32 {
    const rows = grid.len;
    const cols = grid[0].len;
    var pq = std.PriorityQueue(Position, void, lessThan).init(allocator, {});
    defer pq.deinit();

    var dist = try allocator.alloc([]u32, rows);
    defer allocator.free(dist);
    for (dist) |*row| {
        row.* = try allocator.alloc(u32, cols);
        @memset(row.*, math.maxInt(u32));
    }
    defer for (dist) |row| allocator.free(row);

    try pq.add(.{ .x = 0, .y = 0, .risk = 0 });
    dist[0][0] = 0;

    const directions = [_][2]i8{ .{ 1, 0 }, .{ 0, 1 }, .{ -1, 0 }, .{ 0, -1 } };

    while (pq.removeOrNull()) |curr| {
        if (curr.x == rows - 1 and curr.y == cols - 1) {
            return curr.risk;
        }

        for (directions) |d| {
            const nx = @as(i32, curr.x) + d[0];
            const ny = @as(i32, curr.y) + d[1];
            if (nx >= 0 and ny >= 0 and nx < rows and ny < cols) {
                const next_risk = curr.risk + grid[@intCast(nx)][@intCast(ny)];
                if (next_risk < dist[@intCast(nx)][@intCast(ny)]) {
                    dist[@intCast(nx)][@intCast(ny)] = next_risk;
                    try pq.add(.{ .x = @intCast(nx), .y = @intCast(ny), .risk = next_risk });
                }
            }
        }
    }

    return math.maxInt(u32);
}

fn extendGrid(allocator: Allocator, initial_grid: []const []const u8) ![][]u8 {
    const rows = initial_grid.len;
    const cols = initial_grid[0].len;
    var extended_grid = try allocator.alloc([]u8, rows * 5);
    errdefer allocator.free(extended_grid);

    for (extended_grid) |*row| {
        row.* = try allocator.alloc(u8, cols * 5);
    }

    for (0..rows * 5) |i| {
        for (0..cols * 5) |j| {
            var new_risk = initial_grid[i % rows][j % cols] - '0' + @as(u8, @intCast(i / rows + j / cols));
            if (new_risk > 9) new_risk -= 9;
            extended_grid[i][j] = new_risk;
        }
    }

    return extended_grid;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(content);

    var lines = std.mem.split(u8, content, "\n");
    var initial_grid = std.ArrayList([]const u8).init(allocator);
    defer initial_grid.deinit();

    while (lines.next()) |line| {
        if (line.len == 0) continue;
        try initial_grid.append(line);
    }

    var extended_grid = try extendGrid(allocator, initial_grid.items);
    defer {
        for (extended_grid) |row| {
            allocator.free(row);
        }
        allocator.free(extended_grid);
    }

    const result = try dijkstra(allocator, extended_grid);
    try std.io.getStdOut().writer().print("{}\n", .{result});
}
