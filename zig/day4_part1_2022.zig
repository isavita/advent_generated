
const std = @import("std");

fn parseRange(r: []const u8) struct { start: u32, end: u32 } {
    var it = std.mem.split(u8, r, "-");
    return .{
        .start = std.fmt.parseInt(u32, it.next().?, 10) catch unreachable,
        .end = std.fmt.parseInt(u32, it.next().?, 10) catch unreachable,
    };
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(content);

    var count: u32 = 0;
    var lines = std.mem.split(u8, content, "\n");
    while (lines.next()) |line| {
        if (line.len == 0) continue;
        var ranges = std.mem.split(u8, line, ",");
        const range1 = parseRange(ranges.next().?);
        const range2 = parseRange(ranges.next().?);

        if ((range1.start <= range2.start and range1.end >= range2.end) or
            (range2.start <= range1.start and range2.end >= range1.end))
        {
            count += 1;
        }
    }

    const stdout = std.io.getStdOut().writer();
    try stdout.print("{d}\n", .{count});
}
