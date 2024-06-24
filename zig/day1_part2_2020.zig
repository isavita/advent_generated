
const std = @import("std");

pub fn main() !void {
    var file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var expenses = std.ArrayList(u32).init(std.heap.page_allocator);
    defer expenses.deinit();

    var buf: [10]u8 = undefined;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        const num = try std.fmt.parseInt(u32, line, 10);
        try expenses.append(num);
    }

    const result = try findProduct(&expenses);
    try std.io.getStdOut().writer().print("{d}\n", .{result});
}

fn findProduct(expenses: *std.ArrayList(u32)) !u32 {
    const target: u32 = 2020;
    var seen = std.AutoHashMap(u32, void).init(std.heap.page_allocator);
    defer seen.deinit();

    for (expenses.items) |a| {
        for (expenses.items) |b| {
            const c = target -% a -% b;
            if (seen.contains(c)) {
                return a * b * c;
            }
        }
        try seen.put(a, {});
    }

    return error.NotFound;
}
