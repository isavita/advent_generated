const std = @import("std");

const TARGET_VOLUME: u32 = 150;

fn countCombinations(containers: []const u32, index: usize, current_volume: u32, current_count: u32, target_count: u32) u32 {
    if (current_volume == TARGET_VOLUME and current_count == target_count) {
        return 1;
    }
    if (index >= containers.len or current_volume > TARGET_VOLUME or current_count > target_count) {
        return 0;
    }

    return countCombinations(containers, index + 1, current_volume + containers[index], current_count + 1, target_count) +
           countCombinations(containers, index + 1, current_volume, current_count, target_count);
}

fn findMinContainers(containers: []const u32, index: usize, current_volume: u32, current_count: u32) ?u32 {
    if (current_volume == TARGET_VOLUME) {
        return current_count;
    }
    if (index >= containers.len or current_volume > TARGET_VOLUME) {
        return null;
    }

    const with_current = findMinContainers(containers, index + 1, current_volume + containers[index], current_count + 1);
    const without_current = findMinContainers(containers, index + 1, current_volume, current_count);

    if (with_current == null and without_current == null) {
        return null;
    } else if (with_current == null) {
        return without_current;
    } else if (without_current == null) {
        return with_current;
    } else {
        return @min(with_current.?, without_current.?);
    }
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

    const min_containers = findMinContainers(containers.items, 0, 0, 0) orelse {
        std.debug.print("No valid combination found\n", .{});
        return;
    };

    const combinations = countCombinations(containers.items, 0, 0, 0, min_containers);

    const stdout = std.io.getStdOut().writer();
    try stdout.print("Minimum number of containers: {}\n", .{min_containers});
    try stdout.print("Number of ways to use the minimum number of containers: {}\n", .{combinations});
}
