const std = @import("std");

pub fn main() !void {
    var file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var reader = std.io.bufferedReader(file.reader());
    var line_reader = reader.reader();

    var bit_counts = std.ArrayList(u32).init(std.heap.page_allocator);
    defer bit_counts.deinit();

    var total_lines: u32 = 0;

    var buffer: [1024]u8 = undefined;
    while (try line_reader.readUntilDelimiterOrEof(&buffer, '\n')) |line| {
        total_lines += 1;
        for (line, 0..) |char, i| {
            if (i >= bit_counts.items.len) {
                try bit_counts.append(0);
            }
            if (char == '1') {
                bit_counts.items[i] += 1;
            }
        }
    }

    var gamma_rate: u32 = 0;
    var epsilon_rate: u32 = 0;

    for (bit_counts.items) |count| {
        gamma_rate <<= 1;
        epsilon_rate <<= 1;
        if (count > total_lines / 2) {
            gamma_rate |= 1;
        } else {
            epsilon_rate |= 1;
        }
    }

    const power_consumption = gamma_rate * epsilon_rate;
    std.debug.print("{}\n", .{power_consumption});
}