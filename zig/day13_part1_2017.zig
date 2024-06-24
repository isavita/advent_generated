
const std = @import("std");

const Layer = struct {
    depth: u32,
    range: u32,
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var layers = std.ArrayList(Layer).init(allocator);
    defer layers.deinit();

    var buf: [1024]u8 = undefined;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var it = std.mem.split(u8, line, ": ");
        const depth = try std.fmt.parseInt(u32, it.next().?, 10);
        const range = try std.fmt.parseInt(u32, it.next().?, 10);
        try layers.append(Layer{ .depth = depth, .range = range });
    }

    var severity: u32 = 0;
    for (layers.items) |layer| {
        if (layer.depth % (2 * (layer.range - 1)) == 0) {
            severity += layer.depth * layer.range;
        }
    }

    const stdout = std.io.getStdOut().writer();
    try stdout.print("Total severity: {}\n", .{severity});
}
