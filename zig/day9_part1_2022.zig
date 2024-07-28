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

    var tail_positions = std.AutoHashMap(Position, void).init(allocator);
    defer tail_positions.deinit();

    var head = Position{ .x = 0, .y = 0 };
    var tail = Position{ .x = 0, .y = 0 };

    try tail_positions.put(tail, {});

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var buf: [1024]u8 = undefined;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var parts = std.mem.split(u8, line, " ");
        const direction = parts.next().?;
        const steps = try std.fmt.parseInt(i32, parts.next().?, 10);

        var d: Position = undefined;
        if (std.mem.eql(u8, direction, "U")) {
            d = Position{ .x = 0, .y = 1 };
        } else if (std.mem.eql(u8, direction, "D")) {
            d = Position{ .x = 0, .y = -1 };
        } else if (std.mem.eql(u8, direction, "L")) {
            d = Position{ .x = -1, .y = 0 };
        } else if (std.mem.eql(u8, direction, "R")) {
            d = Position{ .x = 1, .y = 0 };
        } else {
            unreachable;
        }

        var i: i32 = 0;
        while (i < steps) : (i += 1) {
            head.x += d.x;
            head.y += d.y;

            if (!isTouching(head, tail)) {
                tail.x += sign(head.x - tail.x);
                tail.y += sign(head.y - tail.y);
            }

            try tail_positions.put(tail, {});
        }
    }

    std.debug.print("{}\n", .{tail_positions.count()});
}

fn isTouching(head: Position, tail: Position) bool {
    return abs(head.x - tail.x) <= 1 and abs(head.y - tail.y) <= 1;
}

fn sign(val: i32) i32 {
    if (val > 0) return 1;
    if (val < 0) return -1;
    return 0;
}

fn abs(val: i32) i32 {
    if (val < 0) return -val;
    return val;
}