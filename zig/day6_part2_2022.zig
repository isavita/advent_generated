const std = @import("std");

pub fn main() !void {
    var file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    const content = try file.readToEndAlloc(std.heap.page_allocator, 1024 * 1024);
    defer std.heap.page_allocator.free(content);

    const part1 = findMarker(content, 4);
    const part2 = findMarker(content, 14);

    try std.io.getStdOut().writer().print("Part 1: {}\n", .{part1});
    try std.io.getStdOut().writer().print("Part 2: {}\n", .{part2});
}

fn findMarker(data: []const u8, markerLength: usize) usize {
    var i: usize = 0;
    while (i + markerLength <= data.len) : (i += 1) {
        var seen = [_]bool{false} ** 256;
        var j: usize = 0;
        while (j < markerLength) : (j += 1) {
            const char = data[i + j];
            if (seen[char]) {
                break;
            }
            seen[char] = true;
        }
        if (j == markerLength) {
            return i + markerLength;
        }
    }
    return 0; // Not found
}