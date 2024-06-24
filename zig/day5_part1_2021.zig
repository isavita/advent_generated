
const std = @import("std");

const Point = struct {
    x: i32,
    y: i32,
};

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var grid = std.AutoHashMap(Point, u16).init(allocator);
    defer grid.deinit();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var buf: [1024]u8 = undefined;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var it = std.mem.split(u8, line, " -> ");
        const start = it.next().?;
        const end = it.next().?;

        var start_it = std.mem.split(u8, start, ",");
        var end_it = std.mem.split(u8, end, ",");

        const x1 = try std.fmt.parseInt(i32, start_it.next().?, 10);
        const y1 = try std.fmt.parseInt(i32, start_it.next().?, 10);
        const x2 = try std.fmt.parseInt(i32, end_it.next().?, 10);
        const y2 = try std.fmt.parseInt(i32, end_it.next().?, 10);

        if (x1 == x2) {
            const min_y = @min(y1, y2);
            const max_y = @max(y1, y2);
            var y: i32 = min_y;
            while (y <= max_y) : (y += 1) {
                const p = Point{ .x = x1, .y = y };
                const gop = try grid.getOrPut(p);
                if (!gop.found_existing) {
                    gop.value_ptr.* = 1;
                } else {
                    gop.value_ptr.* += 1;
                }
            }
        } else if (y1 == y2) {
            const min_x = @min(x1, x2);
            const max_x = @max(x1, x2);
            var x: i32 = min_x;
            while (x <= max_x) : (x += 1) {
                const p = Point{ .x = x, .y = y1 };
                const gop = try grid.getOrPut(p);
                if (!gop.found_existing) {
                    gop.value_ptr.* = 1;
                } else {
                    gop.value_ptr.* += 1;
                }
            }
        }
    }

    var overlap_count: u32 = 0;
    var it = grid.iterator();
    while (it.next()) |entry| {
        if (entry.value_ptr.* > 1) {
            overlap_count += 1;
        }
    }

    try std.io.getStdOut().writer().print("{}\n", .{overlap_count});
}
