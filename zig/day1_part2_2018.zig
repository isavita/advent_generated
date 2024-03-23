const std = @import("std");

pub fn main() !void {
    var file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var reader = std.io.bufferedReader(file.reader());
    var line_reader = reader.reader();

    var frequency_changes = std.ArrayList(i32).init(std.heap.page_allocator);
    defer frequency_changes.deinit();

    var buffer: [1024]u8 = undefined;
    while (try line_reader.readUntilDelimiterOrEof(&buffer, '\n')) |line| {
        if (line.len == 0) continue;
        const frequency_delta = try std.fmt.parseInt(i32, line, 10);
        try frequency_changes.append(frequency_delta);
    }

    var frequencies = std.AutoHashMap(i32, void).init(std.heap.page_allocator);
    defer frequencies.deinit();

    var current_frequency: i32 = 0;
    try frequencies.put(current_frequency, {});

    while (true) {
        for (frequency_changes.items) |change| {
            current_frequency += change;
            if (frequencies.contains(current_frequency)) {
                std.debug.print("{}\n", .{current_frequency});
                return;
            }
            try frequencies.put(current_frequency, {});
        }
    }
}