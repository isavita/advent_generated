const std = @import("std");

fn min(vals: []const i32) i32 {
    var minVal = vals[0];
    for (vals[1..]) |val| {
        if (val < minVal) {
            minVal = val;
        }
    }
    return minVal;
}

pub fn main() !void {
    var file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var reader = std.io.bufferedReader(file.reader());
    var in_stream = reader.reader();

    var total: i32 = 0;
    var buf: [1024]u8 = undefined;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        if (line.len == 0) continue;

        var parts = std.mem.split(u8, line, "x");
        const l = try std.fmt.parseInt(i32, parts.next().?, 10);
        const w = try std.fmt.parseInt(i32, parts.next().?, 10);
        const h = try std.fmt.parseInt(i32, parts.next().?, 10);

        const side1 = l * w;
        const side2 = w * h;
        const side3 = h * l;

        const smallest = min(&[_]i32{ side1, side2, side3 });
        total += 2 * side1 + 2 * side2 + 2 * side3 + smallest;
    }

    std.debug.print("{d}\n", .{total});
}