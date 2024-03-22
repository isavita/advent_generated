const std = @import("std");

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    var file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var reader = file.reader();
    var line_buffer: [1024]u8 = undefined;
    var current_calories: u32 = 0;
    var max_calories: u32 = 0;

    while (try reader.readUntilDelimiterOrEof(&line_buffer, '\n')) |line| {
        if (line.len == 0) {
            if (current_calories > max_calories) {
                max_calories = current_calories;
            }
            current_calories = 0;
        } else {
            current_calories += try std.fmt.parseInt(u32, line, 10);
        }
    }

    if (current_calories > max_calories) {
        max_calories = current_calories;
    }

    std.debug.print("The Elf carrying the most Calories is carrying {} Calories.\n", .{max_calories});
}