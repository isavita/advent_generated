const std = @import("std");

pub fn main() !void {
    var file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    const data = try file.readToEndAlloc(std.heap.page_allocator, 1024 * 1024);
    defer std.heap.page_allocator.free(data);

    var directions = std.mem.trimRight(u8, data, "\n");
    var visitedHouses = std.AutoHashMap([2]i32, bool).init(std.heap.page_allocator);
    defer visitedHouses.deinit();

    var xSanta: i32 = 0;
    var ySanta: i32 = 0;
    var xRobo: i32 = 0;
    var yRobo: i32 = 0;
    var isSantaTurn: bool = true;

    try visitedHouses.put([2]i32{ xSanta, ySanta }, true);

    for (directions) |dir| {
        var x: *i32 = undefined;
        var y: *i32 = undefined;
        if (isSantaTurn) {
            x = &xSanta;
            y = &ySanta;
        } else {
            x = &xRobo;
            y = &yRobo;
        }

        switch (dir) {
            '^' => y.* += 1, // Move north
            'v' => y.* -= 1, // Move south
            '>' => x.* += 1, // Move east
            '<' => x.* -= 1, // Move west
            else => {},
        }

        try visitedHouses.put([2]i32{ x.*, y.* }, true);
        isSantaTurn = !isSantaTurn;
    }

    std.debug.print("{d}\n", .{visitedHouses.count()});
}