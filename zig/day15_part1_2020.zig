const std = @import("std");

pub fn main() !void {
    var file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buffer: [1024]u8 = undefined;
    const bytes_read = try file.readAll(&buffer);
    const input = buffer[0..bytes_read];

    var starting_numbers = std.mem.tokenize(u8, input, ",");
    var last_spoken = std.AutoHashMap(i32, i32).init(std.heap.page_allocator);
    defer last_spoken.deinit();

    var last_number: i32 = 0;
    var next_number: i32 = 0;
    var turn: i32 = 1;

    // Iterate through the starting numbers
    while (starting_numbers.next()) |num| {
        last_number = try std.fmt.parseInt(i32, num, 10);
        try last_spoken.put(last_number, turn);
        turn += 1;
    }

    while (turn <= 2020) : (turn += 1) {
        if (last_spoken.get(last_number)) |last_turn| {
            next_number = turn - 1 - last_turn;
        } else {
            next_number = 0;
        }

        try last_spoken.put(last_number, turn - 1);
        last_number = next_number;
    }

    std.debug.print("{}\n", .{last_number});
}