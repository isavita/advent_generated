
const std = @import("std");
const ArrayList = std.ArrayList;
const AutoHashMap = std.AutoHashMap;

const Point = struct { x: i32, y: i32 };
const State = struct { pos: Point, keys: u32 };

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var grid = ArrayList([]const u8).init(allocator);
    defer grid.deinit();

    var start = Point{ .x = 0, .y = 0 };
    var key_map = AutoHashMap(u8, u5).init(allocator);
    defer key_map.deinit();

    var key_counter: u5 = 0;
    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();
    var buf: [1024]u8 = undefined;
    var y: i32 = 0;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        try grid.append(try allocator.dupe(u8, line));
        for (line, 0..) |char, x| {
            if (char == '@') {
                start = .{ .x = @intCast(x), .y = y };
            } else if (char >= 'a' and char <= 'z') {
                try key_map.put(char, key_counter);
                key_counter += 1;
            }
        }
        y += 1;
    }

    const result = try findShortestPath(allocator, grid.items, start, key_map);
    try std.io.getStdOut().writer().print("{d}\n", .{result});
}

fn findShortestPath(allocator: std.mem.Allocator, grid: []const []const u8, start: Point, key_map: AutoHashMap(u8, u5)) !i32 {
    const dirs = [_]Point{ .{ .x = 0, .y = -1 }, .{ .x = -1, .y = 0 }, .{ .x = 0, .y = 1 }, .{ .x = 1, .y = 0 } };
    var visited = AutoHashMap(State, void).init(allocator);
    defer visited.deinit();

    var queue = ArrayList(State).init(allocator);
    defer queue.deinit();
    try queue.append(.{ .pos = start, .keys = 0 });

    var steps: i32 = 0;
    const all_keys = (@as(u32, 1) << @intCast(key_map.count())) - 1;

    while (queue.items.len > 0) {
        const size = queue.items.len;
        var i: usize = 0;
        while (i < size) : (i += 1) {
            const current = queue.orderedRemove(0);

            if (current.keys == all_keys) {
                return steps;
            }

            for (dirs) |d| {
                const next = Point{ .x = current.pos.x + d.x, .y = current.pos.y + d.y };
                if (next.x >= 0 and next.x < grid[0].len and next.y >= 0 and next.y < grid.len) {
                    const char = grid[@intCast(next.y)][@intCast(next.x)];
                    if (char != '#' and !(char >= 'A' and char <= 'Z' and (current.keys & (@as(u32, 1) << @intCast(key_map.get(char + 32) orelse continue))) == 0)) {
                        var new_state = State{ .pos = next, .keys = current.keys };
                        if (char >= 'a' and char <= 'z') {
                            new_state.keys |= @as(u32, 1) << @intCast(key_map.get(char) orelse continue);
                        }
                        if (!visited.contains(new_state)) {
                            try visited.put(new_state, {});
                            try queue.append(new_state);
                        }
                    }
                }
            }
        }
        steps += 1;
    }
    return -1;
}
