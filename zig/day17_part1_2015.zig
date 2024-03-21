const std = @import("std");

fn countCombinations(containers: []i32, target: i32, index: usize) i32 {
    if (target == 0) return 1;
    if (target < 0 or index >= containers.len) return 0;
    return countCombinations(containers, target - containers[index], index + 1) +
           countCombinations(containers, target, index + 1);
}

pub fn main() !void {
    var file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var containers = std.ArrayList(i32).init(std.heap.page_allocator);
    defer containers.deinit();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var buffer: [1024]u8 = undefined;
    while (true) {
        const line = (try in_stream.readUntilDelimiterOrEof(&buffer, '\n')) orelse break;
        const size = try std.fmt.parseInt(i32, line, 10);
        try containers.append(size);
    }

    std.debug.print("{}\n", .{countCombinations(containers.items, 150, 0)});
}