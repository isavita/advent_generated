const std = @import("std");

pub fn main() !void {
    var file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    const directions = try file.readToEndAlloc(std.heap.page_allocator, 1024 * 1024);
    defer std.heap.page_allocator.free(directions);

    var visited_houses = std.AutoHashMap([2]i32, bool).init(std.heap.page_allocator);
    defer visited_houses.deinit();

    var x: i32 = 0;
    var y: i32 = 0;
    try visited_houses.put([2]i32{ x, y }, true);

    for (directions) |dir| {
        switch (dir) {
            '^' => y += 1, // Move north
            'v' => y -= 1, // Move south
            '>' => x += 1, // Move east
            '<' => x -= 1, // Move west
            else => {},
        }
        try visited_houses.put([2]i32{ x, y }, true);
    }

    std.debug.print("{}\n", .{visited_houses.count()});
}