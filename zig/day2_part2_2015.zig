const std = @import("std");

pub fn main() !void {
    var file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var reader = file.reader();
    var buf: [1024]u8 = undefined;
    var line_reader = std.io.bufferedReader(reader);
    var in = line_reader.reader();

    var total_ribbon: i32 = 0;

    while (try in.readUntilDelimiterOrEof(buf[0..], '\n')) |line| {
        if (line.len == 0) continue;

        var parts = std.mem.split(u8, line, "x");
        const l = try std.fmt.parseInt(i32, parts.next().?, 10);
        const w = try std.fmt.parseInt(i32, parts.next().?, 10);
        const h = try std.fmt.parseInt(i32, parts.next().?, 10);

        const perim1 = 2 * (l + w);
        const perim2 = 2 * (w + h);
        const perim3 = 2 * (h + l);
        const perim = @min(perim1, @min(perim2, perim3));
        const ribbon = perim + l * w * h;

        total_ribbon += ribbon;
    }

    std.debug.print("{}\n", .{total_ribbon});
}
