
const std = @import("std");

const Position = struct {
    x: i32,
    y: i32,
};

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var grid = std.AutoHashMap(Position, bool).init(allocator);
    defer grid.deinit();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var buf: [1024]u8 = undefined;
    var y: i32 = 0;
    var start_x: i32 = 0;
    var start_y: i32 = 0;

    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        for (line, 0..) |c, x| {
            if (c == '#') {
                try grid.put(Position{ .x = @intCast(x), .y = y }, true);
            }
        }
        start_x = @intCast(line.len / 2);
        start_y = @divTrunc(y, 2);
        y += 1;
    }

    const dx = [_]i32{ 0, 1, 0, -1 };
    const dy = [_]i32{ -1, 0, 1, 0 };

    var x: i32 = start_x;
    var y_pos: i32 = start_y;
    var dir: u8 = 0;
    var infected_count: u32 = 0;

    var i: u32 = 0;
    while (i < 10000) : (i += 1) {
        const pos = Position{ .x = x, .y = y_pos };
        if (grid.get(pos)) |_| {
            dir = (dir + 1) % 4;
            _ = grid.remove(pos);
        } else {
            dir = (dir -% 1) % 4;
            try grid.put(pos, true);
            infected_count += 1;
        }
        x += dx[dir];
        y_pos += dy[dir];
    }

    try std.io.getStdOut().writer().print("{}\n", .{infected_count});
}
