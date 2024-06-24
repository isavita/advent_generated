
const std = @import("std");

const hashTableSize = 256;

fn hashString(str: []const u8) u8 {
    var res: u32 = 0;
    for (str) |char| {
        res +%= char;
        res *%= 17;
        res &= 0xFF;
    }
    return @intCast(res);
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(content);

    var result: u32 = 0;
    var it = std.mem.split(u8, std.mem.trimRight(u8, content, &std.ascii.whitespace), ",");
    while (it.next()) |step| {
        result += hashString(step);
    }

    const stdout = std.io.getStdOut().writer();
    try stdout.print("{d}\n", .{result});
}
