const std = @import("std");

const Disc = struct {
    positions: u32,
    start_position: u32,
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var discs = std.ArrayList(Disc).init(allocator);
    defer discs.deinit();

    var buf: [1024]u8 = undefined;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var it = std.mem.split(u8, line, " ");
        _ = it.next(); // "Disc"
        _ = it.next(); // "#1"
        _ = it.next(); // "has"
        const positions = try std.fmt.parseInt(u32, it.next().?, 10);
        _ = it.next(); // "positions;"
        _ = it.next(); // "at"
        _ = it.next(); // "time=0,"
        _ = it.next(); // "it"
        _ = it.next(); // "is"
        _ = it.next(); // "at"
        _ = it.next(); // "position"
        const start_position = try std.fmt.parseInt(u32, it.next().?[0..1], 10);

        try discs.append(Disc{ .positions = positions, .start_position = start_position });
    }

    // Add the new disc for part two
    try discs.append(Disc{ .positions = 11, .start_position = 0 });

    var time: u32 = 0;
    while (true) : (time += 1) {
        var success = true;
        for (discs.items, 0..) |disc, i| {
            const pos = (disc.start_position + time + i + 1) % disc.positions;
            if (pos != 0) {
                success = false;
                break;
            }
        }
        if (success) {
            break;
        }
    }

    const stdout = std.io.getStdOut().writer();
    try stdout.print("First time to press the button: {}\n", .{time});
}