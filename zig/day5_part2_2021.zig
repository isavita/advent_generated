const std = @import("std");

fn abs(x: i32) i32 {
    if (x < 0) return -x else return x;
}

fn sign(x: i32) i32 {
    if (x > 0) return 1 else if (x < 0) return -1 else return 0;
}

pub fn main() !void {
    var file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var reader = std.io.bufferedReader(file.reader());
    var line_buffer: [1024]u8 = undefined;
    var line_reader = reader.reader();

    var lines = std.ArrayList([4]i32).init(std.heap.page_allocator);
    defer lines.deinit();

    while (try line_reader.readUntilDelimiterOrEof(&line_buffer, '\n')) |line| {
        var parts = std.mem.split(u8, line, " -> ");
        var start = std.mem.split(u8, parts.next().?, ",");
        var end = std.mem.split(u8, parts.next().?, ",");

        const x1 = try std.fmt.parseInt(i32, start.next().?, 10);
        const y1 = try std.fmt.parseInt(i32, start.next().?, 10);
        const x2 = try std.fmt.parseInt(i32, end.next().?, 10);
        const y2 = try std.fmt.parseInt(i32, end.next().?, 10);

        try lines.append([4]i32{ x1, y1, x2, y2 });
    }

    var overlaps = std.AutoHashMap([2]i32, i32).init(std.heap.page_allocator);
    defer overlaps.deinit();

    for (lines.items) |line| {
        const x1 = line[0];
        const y1 = line[1];
        const x2 = line[2];
        const y2 = line[3];

        const xStep = sign(x2 - x1);
        const yStep = sign(y2 - y1);
        var steps = abs(x2 - x1) + 1;
        if (abs(y2 - y1) > abs(x2 - x1)) {
            steps = abs(y2 - y1) + 1;
        }

        var i: i32 = 0;
        while (i < steps) : (i += 1) {
            const point = [2]i32{ x1 + i * xStep, y1 + i * yStep };
            const count = overlaps.get(point) orelse 0;
            try overlaps.put(point, count + 1);
        }
    }

    var count: i32 = 0;
    var it = overlaps.iterator();
    while (it.next()) |entry| {
        if (entry.value_ptr.* > 1) {
            count += 1;
        }
    }

    std.debug.print("{}\n", .{count});
}