
const std = @import("std");
const fs = std.fs;
const io = std.io;
const mem = std.mem;
const math = std.math;

pub fn main() !void {
    const file = try fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buf_reader = io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var buf: [1024]u8 = undefined;
    var checksum: u32 = 0;
    var divisible_sum: u32 = 0;

    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var iter = mem.split(u8, line, "\t");
        var min: u32 = math.maxInt(u32);
        var max: u32 = 0;
        var numbers = std.ArrayList(u32).init(std.heap.page_allocator);
        defer numbers.deinit();

        while (iter.next()) |num_str| {
            const num = try std.fmt.parseInt(u32, num_str, 10);
            try numbers.append(num);
            min = @min(min, num);
            max = @max(max, num);
        }

        checksum += max - min;

        for (numbers.items) |a| {
            for (numbers.items) |b| {
                if (a != b and a % b == 0) {
                    divisible_sum += a / b;
                }
            }
        }
    }

    try std.io.getStdOut().writer().print("Part 1: {}\n", .{checksum});
    try std.io.getStdOut().writer().print("Part 2: {}\n", .{divisible_sum});
}
