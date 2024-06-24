
const std = @import("std");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(content);

    const slopes = [_][2]u8{ .{ 1, 1 }, .{ 3, 1 }, .{ 5, 1 }, .{ 7, 1 }, .{ 1, 2 } };
    var product: u64 = 1;

    var line_start: usize = 0;
    var line_end: usize = 0;
    var line_width: usize = 0;

    while (line_end < content.len) : (line_end += 1) {
        if (content[line_end] == '\n') {
            if (line_width == 0) line_width = line_end - line_start;
            line_start = line_end + 1;
        }
    }

    for (slopes) |slope| {
        var tree_count: u32 = 0;
        var pos: usize = 0;
        var i: usize = 0;

        while (i < content.len) : (i += @as(usize, slope[1]) * (line_width + 1)) {
            if (content[i + pos] == '#') tree_count += 1;
            pos = (pos + slope[0]) % line_width;
        }

        product *= tree_count;
    }

    try std.io.getStdOut().writer().print("{d}\n", .{product});
}
