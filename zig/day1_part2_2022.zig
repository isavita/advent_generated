
const std = @import("std");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var calories_list = std.ArrayList(u32).init(allocator);
    defer calories_list.deinit();

    var current_calories: u32 = 0;
    var buf: [100]u8 = undefined;

    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        if (line.len == 0) {
            try calories_list.append(current_calories);
            current_calories = 0;
            continue;
        }

        current_calories += try std.fmt.parseInt(u32, line, 10);
    }

    try calories_list.append(current_calories);

    std.mem.sort(u32, calories_list.items, {}, comptime std.sort.desc(u32));

    var top_three_sum: u32 = 0;
    for (calories_list.items[0..@min(3, calories_list.items.len)]) |calories| {
        top_three_sum += calories;
    }

    const stdout = std.io.getStdOut().writer();
    try stdout.print("{}\n", .{top_three_sum});
}
