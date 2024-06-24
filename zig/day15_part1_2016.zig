
const std = @import("std");
const fs = std.fs;
const io = std.io;
const mem = std.mem;
const fmt = std.fmt;

const Disc = struct {
    positions: u32,
    start_position: u32,
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file = try fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buf_reader = io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var discs = std.ArrayList(Disc).init(allocator);
    defer discs.deinit();

    var buf: [1024]u8 = undefined;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var it = mem.split(u8, line, " ");
        _ = it.next(); // Skip "Disc"
        _ = it.next(); // Skip "#X"
        _ = it.next(); // Skip "has"
        const positions = try fmt.parseInt(u32, it.next().?, 10);
        _ = it.next(); // Skip "positions;"
        _ = it.next(); // Skip "at"
        _ = it.next(); // Skip "time=0,"
        _ = it.next(); // Skip "it"
        _ = it.next(); // Skip "is"
        _ = it.next(); // Skip "at"
        _ = it.next(); // Skip "position"
        const start_position = try fmt.parseInt(u32, it.next().?[0..1], 10);

        try discs.append(Disc{ .positions = positions, .start_position = start_position });
    }

    const result = findFirstValidTime(discs.items);
    try std.io.getStdOut().writer().print("The first valid time is: {d}\n", .{result});
}

fn findFirstValidTime(discs: []const Disc) u32 {
    var time: u32 = 0;
    while (true) : (time += 1) {
        var valid = true;
        for (discs, 0..) |disc, i| {
            const position = (disc.start_position + time + i + 1) % disc.positions;
            if (position != 0) {
                valid = false;
                break;
            }
        }
        if (valid) {
            return time;
        }
    }
}
