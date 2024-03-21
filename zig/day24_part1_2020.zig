const std = @import("std");

const Coordinate = struct {
    q: i32,
    r: i32,
};

const directions = [6]Coordinate{
    .{ .q = 1, .r = 0 },
    .{ .q = 0, .r = 1 },
    .{ .q = -1, .r = 1 },
    .{ .q = -1, .r = 0 },
    .{ .q = 0, .r = -1 },
    .{ .q = 1, .r = -1 },
};

pub fn main() !void {
    var file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var blackTiles = std.AutoHashMap(Coordinate, bool).init(std.heap.page_allocator);
    defer blackTiles.deinit();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var buf: [1024]u8 = undefined;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var coord = Coordinate{ .q = 0, .r = 0 };
        var i: usize = 0;
        while (i < line.len) {
            var dir: Coordinate = undefined;
            if (line[i] == 'e') {
                dir = directions[0];
            } else if (line[i] == 'w') {
                dir = directions[3];
            } else if (line[i] == 'n' or line[i] == 's') {
                if (line.len > i + 1) {
                    if (std.mem.eql(u8, line[i .. i + 2], "ne")) {
                        dir = directions[5];
                    } else if (std.mem.eql(u8, line[i .. i + 2], "nw")) {
                        dir = directions[4];
                    } else if (std.mem.eql(u8, line[i .. i + 2], "se")) {
                        dir = directions[1];
                    } else if (std.mem.eql(u8, line[i .. i + 2], "sw")) {
                        dir = directions[2];
                    } else {
                        return error.InvalidInput;
                    }
                    i += 1;
                } else {
                    return error.InvalidInput;
                }
            } else {
                return error.InvalidInput;
            }
            coord.q += dir.q;
            coord.r += dir.r;
            i += 1;
        }

        if (blackTiles.get(coord)) |black| {
            blackTiles.put(coord, !black) catch unreachable;
        } else {
            blackTiles.put(coord, true) catch unreachable;
        }
    }

    var count: usize = 0;
    var it = blackTiles.iterator();
    while (it.next()) |entry| {
        if (entry.value_ptr.*) {
            count += 1;
        }
    }

    std.debug.print("{}\n", .{count});
}