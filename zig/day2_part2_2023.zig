
const std = @import("std");

pub fn main() !void {
    var file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var buf: [1024]u8 = undefined;
    var total_power: u32 = 0;

    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var max_red: u8 = 0;
        var max_green: u8 = 0;
        var max_blue: u8 = 0;

        var it = std.mem.split(u8, line, ": ");
        _ = it.next();
        if (it.next()) |rounds| {
            var round_it = std.mem.split(u8, rounds, "; ");
            while (round_it.next()) |round| {
                var cube_it = std.mem.split(u8, round, ", ");
                while (cube_it.next()) |cube| {
                    var cube_parts = std.mem.split(u8, cube, " ");
                    if (cube_parts.next()) |count_str| {
                        const count = try std.fmt.parseInt(u8, count_str, 10);
                        if (cube_parts.next()) |color| {
                            switch (color[0]) {
                                'r' => max_red = @max(max_red, count),
                                'g' => max_green = @max(max_green, count),
                                'b' => max_blue = @max(max_blue, count),
                                else => {},
                            }
                        }
                    }
                }
            }
        }

        total_power += @as(u32, max_red) * @as(u32, max_green) * @as(u32, max_blue);
    }

    const stdout = std.io.getStdOut().writer();
    try stdout.print("{d}\n", .{total_power});
}
