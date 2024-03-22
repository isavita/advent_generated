const std = @import("std");

fn parseRange(s: []const u8) [2]i32 {
    var it = std.mem.tokenize(u8, s, "-");
    const start = std.fmt.parseInt(i32, it.next().?, 10) catch unreachable;
    const end = std.fmt.parseInt(i32, it.next().?, 10) catch unreachable;
    return [2]i32{ start, end };
}

pub fn main() !void {
    var file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var reader = std.io.bufferedReader(file.reader());
    var buffer: [1024]u8 = undefined;
    var line_reader = reader.reader();

    var count: usize = 0;
    while (try line_reader.readUntilDelimiterOrEof(&buffer, '\n')) |line| {
        var it = std.mem.tokenize(u8, line, ",");
        const left = parseRange(it.next().?);
        const right = parseRange(it.next().?);

        if (left[0] <= right[1] and left[1] >= right[0]) {
            count += 1;
        }
    }

    std.debug.print("{}\n", .{count});
}