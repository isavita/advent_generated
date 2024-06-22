const std = @import("std");

const TARGET_VOLUME: u32 = 150;

fn countCombinations(containers: []const u32, index: usize, current_volume: u32) u32 {
    if (current_volume == TARGET_VOLUME) {
        return 1;
    }
    if (index >= containers.len or current_volume > TARGET_VOLUME) {
        return 0;
    }

    // Count combinations including and excluding the current container
    return countCombinations(containers, index + 1, current_volume + containers[index]) +
           countCombinations(containers, index + 1, current_volume);
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var containers = std.ArrayList(u32).init(allocator);
    defer containers.deinit();

    var buf: [1024]u8 = undefined;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        const container_size = try std.fmt.parseInt(u32, line, 10);
        try containers.append(container_size);
    }

    const combinations = countCombinations(containers.items, 0, 0);

    const stdout = std.io.getStdOut().writer();
    try stdout.print("Number of combinations that can fit exactly 150 liters: {}\n", .{combinations});
}
