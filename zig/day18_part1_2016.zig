const std = @import("std");

pub fn main() !void {
    var file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var buf: [1024]u8 = undefined;
    const row = try in_stream.readUntilDelimiterOrEof(&buf, '\n');
    if (row) |r| {
        var current_row = r;
        var safe_tiles: usize = 0;

        for (current_row) |tile| {
            if (tile == '.') {
                safe_tiles += 1;
            }
        }

        var i: usize = 1;
        while (i < 40) : (i += 1) {
            var next_row = std.ArrayList(u8).init(std.heap.page_allocator);
            defer next_row.deinit();

            for (current_row, 0..) |tile, j| {
                const left = if (j == 0) '.' else current_row[j - 1];
                const center = tile;
                const right = if (j == current_row.len - 1) '.' else current_row[j + 1];

                if ((left == '^' and center == '^' and right == '.') or
                    (left == '.' and center == '^' and right == '^') or
                    (left == '^' and center == '.' and right == '.') or
                    (left == '.' and center == '.' and right == '^'))
                {
                    try next_row.append('^');
                } else {
                    try next_row.append('.');
                    safe_tiles += 1;
                }
            }

            current_row = try next_row.toOwnedSlice();
        }

        std.debug.print("{d}\n", .{safe_tiles});
    }
}