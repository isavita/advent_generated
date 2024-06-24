
const std = @import("std");

pub fn main() !void {
    var file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var buf: [11]u8 = undefined;
    var max_seat_id: u16 = 0;

    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var seat_id: u16 = 0;
        for (line) |c| {
            seat_id <<= 1;
            seat_id |= @as(u16, switch (c) {
                'B', 'R' => 1,
                else => 0,
            });
        }
        max_seat_id = @max(max_seat_id, seat_id);
    }

    const stdout = std.io.getStdOut().writer();
    try stdout.print("{d}\n", .{max_seat_id});
}
