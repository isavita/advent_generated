const std = @import("std");

pub fn main() !void {
    var file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var numbers = std.ArrayList(i32).init(std.heap.page_allocator);
    defer numbers.deinit();

    var buf: [1024]u8 = undefined;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        const num = try std.fmt.parseInt(i32, line, 10);
        try numbers.append(num);
    }

    const target: i32 = 2020;
    var seen = std.AutoHashMap(i32, void).init(std.heap.page_allocator);
    defer seen.deinit();

    for (numbers.items) |num| {
        const complement = target - num;
        if (seen.contains(complement)) {
            const product = num * complement;
            try std.io.getStdOut().writer().print("{d}\n", .{product});
            return;
        }
        try seen.put(num, {});
    }
}