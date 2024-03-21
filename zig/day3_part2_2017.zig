const std = @import("std");

pub fn main() !void {
    var file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buffer: [1024]u8 = undefined;
    const bytes_read = try file.readAll(&buffer);
    const target = try std.fmt.parseInt(i32, std.mem.trimRight(u8, buffer[0..bytes_read], "\n"), 10);

    var grid = std.AutoHashMap([2]i32, i32).init(std.heap.page_allocator);
    defer grid.deinit();

    try grid.put([2]i32{ 0, 0 }, 1);

    var x: i32 = 0;
    var y: i32 = 0;
    var dx: i32 = 0;
    var dy: i32 = -1;

    while (true) {
        // Change direction when reaching a corner
        if (x == y or (x < 0 and x == -y) or (x > 0 and x == 1 - y)) {
            const temp = dx;
            dx = -dy;
            dy = temp;
        }

        // Move to the next square
        x += dx;
        y += dy;

        // Calculate value for the current square
        var value: i32 = 0;
        var dx_offset: i32 = -1;
        while (dx_offset <= 1) : (dx_offset += 1) {
            var dy_offset: i32 = -1;
            while (dy_offset <= 1) : (dy_offset += 1) {
                const key = [2]i32{ x + dx_offset, y + dy_offset };
                value += if (grid.contains(key)) grid.get(key).? else 0;
            }
        }
        try grid.put([2]i32{ x, y }, value);

        // Check if value is greater than the target
        if (value > target) {
            std.debug.print("{}", .{value});
            return;
        }
    }
}