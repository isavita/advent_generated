
const std = @import("std");
const math = std.math;
const PriorityQueue = std.PriorityQueue;

const Position = struct {
    x: usize,
    y: usize,
    risk: u32,
};

fn lessThan(context: void, a: Position, b: Position) std.math.Order {
    _ = context;
    return std.math.order(a.risk, b.risk);
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var grid = std.ArrayList([]u8).init(allocator);
    defer {
        for (grid.items) |row| {
            allocator.free(row);
        }
        grid.deinit();
    }

    var buf: [1024]u8 = undefined;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var row = try allocator.alloc(u8, line.len);
        @memcpy(row, line);
        try grid.append(row);
    }

    const rows = grid.items.len;
    const cols = grid.items[0].len;

    var pq = PriorityQueue(Position, void, lessThan).init(allocator, {});
    defer pq.deinit();

    try pq.add(.{ .x = 0, .y = 0, .risk = 0 });

    var dist = try allocator.alloc([]u32, rows);
    defer allocator.free(dist);
    for (dist) |*row| {
        row.* = try allocator.alloc(u32, cols);
        @memset(row.*, math.maxInt(u32));
    }
    defer {
        for (dist) |row| {
            allocator.free(row);
        }
    }
    dist[0][0] = 0;

    const directions = [_][2]i32{ .{ 1, 0 }, .{ 0, 1 }, .{ -1, 0 }, .{ 0, -1 } };

    while (pq.removeOrNull()) |curr| {
        if (curr.x == rows - 1 and curr.y == cols - 1) {
            const stdout = std.io.getStdOut().writer();
            try stdout.print("{d}\n", .{curr.risk});
            return;
        }

        for (directions) |d| {
            const nx = @as(i32, @intCast(curr.x)) + d[0];
            const ny = @as(i32, @intCast(curr.y)) + d[1];
            if (nx >= 0 and ny >= 0 and nx < rows and ny < cols) {
                const ux = @as(usize, @intCast(nx));
                const uy = @as(usize, @intCast(ny));
                const next_risk = curr.risk + (grid.items[ux][uy] - '0');
                if (next_risk < dist[ux][uy]) {
                    dist[ux][uy] = next_risk;
                    try pq.add(.{ .x = ux, .y = uy, .risk = next_risk });
                }
            }
        }
    }

    const stdout = std.io.getStdOut().writer();
    try stdout.print("-1\n", .{});
}
