
const std = @import("std");
const Point = struct { x: i32, y: i32 };
const DoorMap = std.AutoHashMap(Point, std.AutoHashMap(Point, void));

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    const regex = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(regex);

    var dm = try buildMap(allocator, regex[1 .. regex.len - 1]);
    defer dm.deinit();

    const rooms = try countRooms(allocator, dm, 1000);
    try std.io.getStdOut().writer().print("{d}\n", .{rooms});
}

fn buildMap(allocator: std.mem.Allocator, regex: []const u8) !DoorMap {
    var dm = DoorMap.init(allocator);
    var stack = std.ArrayList(Point).init(allocator);
    defer stack.deinit();

    var cp = Point{ .x = 0, .y = 0 };

    for (regex) |c| {
        switch (c) {
            '(' => try stack.append(cp),
            '|' => cp = stack.items[stack.items.len - 1],
            ')' => cp = stack.pop(),
            'N', 'S', 'E', 'W' => {
                const np = move(cp, c);
                var entry = try dm.getOrPut(cp);
                if (!entry.found_existing) {
                    entry.value_ptr.* = std.AutoHashMap(Point, void).init(allocator);
                }
                try entry.value_ptr.*.put(np, {});
                cp = np;
            },
            else => {},
        }
    }
    return dm;
}

fn move(p: Point, dir: u8) Point {
    return switch (dir) {
        'N' => .{ .x = p.x, .y = p.y - 1 },
        'S' => .{ .x = p.x, .y = p.y + 1 },
        'E' => .{ .x = p.x + 1, .y = p.y },
        'W' => .{ .x = p.x - 1, .y = p.y },
        else => p,
    };
}

fn countRooms(allocator: std.mem.Allocator, dm: DoorMap, min_doors: u32) !u32 {
    var visited = std.AutoHashMap(Point, u32).init(allocator);
    defer visited.deinit();

    var queue = std.ArrayList(Point).init(allocator);
    defer queue.deinit();

    try queue.append(.{ .x = 0, .y = 0 });
    try visited.put(.{ .x = 0, .y = 0 }, 0);

    var room_count: u32 = 0;

    while (queue.items.len > 0) {
        const p = queue.orderedRemove(0);
        const doors = visited.get(p).?;

        if (dm.get(p)) |neighbors| {
            var it = neighbors.keyIterator();
            while (it.next()) |np| {
                if (!visited.contains(np.*)) {
                    const new_doors = doors + 1;
                    try visited.put(np.*, new_doors);
                    if (new_doors >= min_doors) {
                        room_count += 1;
                    }
                    try queue.append(np.*);
                }
            }
        }
    }

    return room_count;
}
