const std = @import("std");

pub fn main() !void {
    var file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var numbers = std.ArrayList(i64).init(std.heap.page_allocator);
    defer numbers.deinit();

    var buf: [1024]u8 = undefined;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        if (line.len == 0) continue;
        const num = try std.fmt.parseInt(i64, line, 10);
        try numbers.append(num);
    }

    const preamble = 25;
    var i: usize = preamble;
    while (i < numbers.items.len) : (i += 1) {
        if (!isValid(numbers.items, i, preamble)) {
            std.debug.print("{}\n", .{numbers.items[i]});
            return;
        }
    }
}

fn isValid(numbers: []const i64, index: usize, preamble: usize) bool {
    const target = numbers[index];
    var i: usize = index - preamble;
    while (i < index) : (i += 1) {
        var j: usize = i + 1;
        while (j < index) : (j += 1) {
            if (numbers[i] + numbers[j] == target) {
                return true;
            }
        }
    }
    return false;
}