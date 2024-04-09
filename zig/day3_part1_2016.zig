const std = @import("std");

pub fn main() !void {
    var file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var reader = std.io.bufferedReader(file.reader());
    var line_reader = reader.reader();

    var valid_triangle_count: usize = 0;

    var buffer: [1024]u8 = undefined;
    while (try line_reader.readUntilDelimiterOrEof(&buffer, '\n')) |line| {
        // Split the line by spaces to extract side lengths
        var sides = std.mem.split(u8, line, " ");
        var side_lengths: [3]i32 = undefined;
        var index: usize = 0;

        // Parse each side length and store in side_lengths array
        while (sides.next()) |side| {
            if (side.len == 0) continue; // Skip empty splits
            side_lengths[index] = try std.fmt.parseInt(i32, side, 10);
            index += 1;
        }

        // Check the triangle inequality theorem
        if (side_lengths[0] + side_lengths[1] > side_lengths[2] and
            side_lengths[1] + side_lengths[2] > side_lengths[0] and
            side_lengths[2] + side_lengths[0] > side_lengths[1]) {
            valid_triangle_count += 1;
        }
    }

    std.debug.print("Valid triangles: {d}\n", .{valid_triangle_count});
}