const std = @import("std");

pub fn main() !void {
    var file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var buf: [1024]u8 = undefined;
    var total_possible_games: u32 = 0;

    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var parts = std.mem.split(u8, line, ": ");
        const game_id_str = parts.next().?;
        const game_id = try std.fmt.parseInt(u32, game_id_str[5..], 10);

        var possible = true;
        var max_red: u8 = 0;
        var max_green: u8 = 0;
        var max_blue: u8 = 0;

        if (parts.next()) |draws| {
            var draw_parts = std.mem.split(u8, draws, "; ");
            while (draw_parts.next()) |draw| {
                var cube_parts = std.mem.split(u8, draw, ", ");
                while (cube_parts.next()) |cube| {
                    var color_parts = std.mem.split(u8, cube, " ");
                    const count = try std.fmt.parseInt(u8, color_parts.next().?, 10);
                    const color = color_parts.next().?;

                    switch (color[0]) {
                        'r' => max_red = @max(max_red, count),
                        'g' => max_green = @max(max_green, count),
                        'b' => max_blue = @max(max_blue, count),
                        else => {},
                    }
                }
            }

            if (max_red > 12 or max_green > 13 or max_blue > 14) {
                possible = false;
            }
        }

        if (possible) {
            total_possible_games += game_id;
        }
    }

    std.debug.print("{}\n", .{total_possible_games});
}