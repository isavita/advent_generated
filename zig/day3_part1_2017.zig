
const std = @import("std");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(content);

    const target = try std.fmt.parseInt(u32, std.mem.trim(u8, content, &std.ascii.whitespace), 10);

    const sideLength = @as(u32, @intCast(std.math.sqrt(target - 1) + 1)) | 1;
    const stepsFromEdge = sideLength >> 1;
    const maxValue = sideLength * sideLength;

    var distanceToMiddle: u32 = std.math.maxInt(u32);
    var i: u32 = 0;
    while (i < 4) : (i += 1) {
        const middlePoint = maxValue - stepsFromEdge - (sideLength - 1) * i;
        const distance = if (target > middlePoint) target - middlePoint else middlePoint - target;
        distanceToMiddle = @min(distanceToMiddle, distance);
    }

    const manhattanDistance = stepsFromEdge + distanceToMiddle;
    try std.io.getStdOut().writer().print("{d}\n", .{manhattanDistance});
}
