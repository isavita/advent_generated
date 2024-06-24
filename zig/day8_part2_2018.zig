const std = @import("std");
const ArrayList = std.ArrayList;

fn readInput(allocator: std.mem.Allocator) !ArrayList(u32) {
    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();
    var buf: [1024]u8 = undefined;

    var numbers = ArrayList(u32).init(allocator);
    while (try in_stream.readUntilDelimiterOrEof(&buf, ' ')) |num_str| {
        const num = try std.fmt.parseInt(u32, num_str, 10);
        try numbers.append(num);
    }
    return numbers;
}

fn parseTree(data: []const u32, index: *usize) u32 {
    const child_count = data[index.*];
    const meta_count = data[index.* + 1];
    index.* += 2;

    var child_values: [16]u32 = undefined;
    var value: u32 = 0;

    if (child_count == 0) {
        var i: usize = 0;
        while (i < meta_count) : (i += 1) {
            value += data[index.* + i];
        }
    } else {
        var i: usize = 0;
        while (i < child_count) : (i += 1) {
            child_values[i] = parseTree(data, index);
        }
        i = 0;
        while (i < meta_count) : (i += 1) {
            const metadata = data[index.* + i];
            if (metadata <= child_count and metadata > 0) {
                value += child_values[metadata - 1];
            }
        }
    }
    index.* += meta_count;
    return value;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var numbers = try readInput(allocator);
    defer numbers.deinit();

    var index: usize = 0;
    const result = parseTree(numbers.items, &index);
    try std.io.getStdOut().writer().print("{}\n", .{result});
}