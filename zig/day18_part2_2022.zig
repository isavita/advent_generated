const std = @import("std");

const Pt3 = struct {
    x: i32,
    y: i32,
    z: i32,
};

fn addPt3(p1: Pt3, p2: Pt3) Pt3 {
    return Pt3{ .x = p1.x + p2.x, .y = p1.y + p2.y, .z = p1.z + p2.z };
}

fn minInt(a: i32, b: i32) i32 {
    return if (a < b) a else b;
}

fn maxInt(a: i32, b: i32) i32 {
    return if (a > b) a else b;
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    var cubes = std.AutoHashMap(Pt3, void).init(allocator);
    defer cubes.deinit();

    const neighbors = [6]Pt3{
        Pt3{ .x = -1, .y = 0, .z = 0 },
        Pt3{ .x = 1, .y = 0, .z = 0 },
        Pt3{ .x = 0, .y = -1, .z = 0 },
        Pt3{ .x = 0, .y = 1, .z = 0 },
        Pt3{ .x = 0, .y = 0, .z = -1 },
        Pt3{ .x = 0, .y = 0, .z = 1 },
    };

    var min_pt = Pt3{ .x = std.math.maxInt(i32), .y = std.math.maxInt(i32), .z = std.math.maxInt(i32) };
    var max_pt = Pt3{ .x = std.math.minInt(i32), .y = std.math.minInt(i32), .z = std.math.minInt(i32) };

    var file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var reader = file.reader();
    var buf: [1024]u8 = undefined;
    var line_reader = std.io.bufferedReader(reader);
    var in = line_reader.reader();

    while (try in.readUntilDelimiterOrEof(buf[0..], '\n')) |line| {
        if (line.len == 0) continue;

        var parts = std.mem.split(u8, line, ",");
        const x = try std.fmt.parseInt(i32, parts.next().?, 10);
        const y = try std.fmt.parseInt(i32, parts.next().?, 10);
        const z = try std.fmt.parseInt(i32, parts.next().?, 10);

        const cube = Pt3{ .x = x, .y = y, .z = z };
        try cubes.put(cube, {});

        min_pt = Pt3{
            .x = minInt(min_pt.x, cube.x),
            .y = minInt(min_pt.y, cube.y),
            .z = minInt(min_pt.z, cube.z),
        };
        max_pt = Pt3{
            .x = maxInt(max_pt.x, cube.x),
            .y = maxInt(max_pt.y, cube.y),
            .z = maxInt(max_pt.z, cube.z),
        };
    }

    min_pt = addPt3(min_pt, Pt3{ .x = -1, .y = -1, .z = -1 });
    max_pt = addPt3(max_pt, Pt3{ .x = 1, .y = 1, .z = 1 });

    var faces: usize = 0;
    var queue = std.ArrayList(Pt3).init(allocator);
    defer queue.deinit();

    var seen = std.AutoHashMap(Pt3, void).init(allocator);
    defer seen.deinit();

    try seen.put(min_pt, {});
    try queue.append(min_pt);

    while (queue.items.len > 0) {
        const curr = queue.pop();

        for (neighbors) |delta| {
            const next = addPt3(curr, delta);
            if (next.x < min_pt.x or
                next.y < min_pt.y or
                next.z < min_pt.z or
                next.x > max_pt.x or
                next.y > max_pt.y or
                next.z > max_pt.z)
            {
                continue;
            }

            if (cubes.contains(next)) {
                faces += 1;
            } else if (!seen.contains(next)) {
                try seen.put(next, {});
                try queue.append(next);
            }
        }
    }

    std.debug.print("{d}\n", .{faces});
}