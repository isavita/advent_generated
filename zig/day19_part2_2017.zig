
const std = @import("std");
const fs = std.fs;
const io = std.io;
const mem = std.mem;

const Direction = enum {
    Up,
    Down,
    Left,
    Right,
};

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const file = try fs.cwd().openFile("input.txt", .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(content);

    var lines = mem.split(u8, content, "\n");
    var grid = std.ArrayList([]const u8).init(allocator);
    while (lines.next()) |line| {
        try grid.append(line);
    }

    var x: usize = mem.indexOfScalar(u8, grid.items[0], '|').?;
    var y: usize = 0;
    var dir = Direction.Down;
    var letters = std.ArrayList(u8).init(allocator);
    var steps: usize = 0;

    while (true) : (steps += 1) {
        const current = grid.items[y][x];
        switch (current) {
            '|', '-' => {},
            '+' => {
                if (dir == .Up or dir == .Down) {
                    if (x > 0 and grid.items[y][x - 1] != ' ') {
                        dir = .Left;
                    } else {
                        dir = .Right;
                    }
                } else {
                    if (y > 0 and grid.items[y - 1][x] != ' ') {
                        dir = .Up;
                    } else {
                        dir = .Down;
                    }
                }
            },
            'A'...'Z' => try letters.append(current),
            ' ' => break,
            else => unreachable,
        }

        switch (dir) {
            .Up => y -= 1,
            .Down => y += 1,
            .Left => x -= 1,
            .Right => x += 1,
        }
    }

    const stdout = io.getStdOut().writer();
    try stdout.print("Part 1: {s}\n", .{letters.items});
    try stdout.print("Part 2: {d}\n", .{steps});
}
