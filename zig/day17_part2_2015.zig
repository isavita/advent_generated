const std = @import("std");

fn findCombinations(containers: []i32, target: i32, index: usize, count: usize, minCount: *usize, ways: *usize) void {
    if (target == 0) {
        if (minCount.* == 0 or count < minCount.*) {
            minCount.* = count;
            ways.* = 1;
        } else if (count == minCount.*) {
            ways.* += 1;
        }
        return;
    }
    if (target < 0 or index >= containers.len) {
        return;
    }
    // Include current container
    findCombinations(containers, target - containers[index], index + 1, count + 1, minCount, ways);
    // Exclude current container
    findCombinations(containers, target, index + 1, count, minCount, ways);
}

pub fn main() !void {
    var file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var containers = std.ArrayList(i32).init(std.heap.page_allocator);
    defer containers.deinit();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();
    var buf: [1024]u8 = undefined;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        if (line.len == 0) {
            break;
        }
        const size = try std.fmt.parseInt(i32, line, 10);
        try containers.append(size);
    }

    var minCount: usize = 0;
    var ways: usize = 0;
    findCombinations(containers.items, 150, 0, 0, &minCount, &ways);
    std.debug.print("{}\n", .{ways});
}