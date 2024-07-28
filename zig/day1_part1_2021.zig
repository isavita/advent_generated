const std = @import("std");

pub fn main() !void {
    var file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var previous_depth: ?u32 = null;
    var increases: u32 = 0;

    var buf: [20]u8 = undefined;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        const depth = try std.fmt.parseInt(u32, line, 10);
        if (previous_depth) |prev| {
            if (depth > prev) {
                increases += 1;
            }
        }
        previous_depth = depth;
    }

    std.debug.print("{}\n", .{increases});
}