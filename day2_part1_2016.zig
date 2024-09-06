const std = @import("std");

fn move_button(curr: u8, direction: u8) u8 {
    switch (direction) {
        'U' => return switch (curr) {
            1, 2, 3 => curr,
            4 => 1,
            5 => 2,
            6 => 3,
            7 => 4,
            8 => 5,
            9 => 6,
            else => unreachable,
        },
        'D' => return switch (curr) {
            7, 8, 9 => curr,
            1 => 4,
            2 => 5,
            3 => 6,
            4 => 7,
            5 => 8,
            6 => 9,
            else => unreachable,
        },
        'L' => return switch (curr) {
            1, 4, 7 => curr,
            2 => 1,
            3 => 2,
            5 => 4,
            6 => 5,
            8 => 7,
            9 => 8,
            else => unreachable,
        },
        'R' => return switch (curr) {
            3, 6, 9 => curr,
            1 => 2,
            2 => 3,
            4 => 5,
            5 => 6,
            7 => 8,
            8 => 9,
            else => unreachable,
        },
        else => return curr,
    }
}

pub fn main() !void {
    var file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var buf: [1024]u8 = undefined;
    var curr_button: u8 = 5;
    var code: [10]u8 = undefined;
    var code_idx: usize = 0;

    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var i: usize = 0;
        while (i < line.len) : (i += 1) {
            curr_button = move_button(curr_button, line[i]);
        }
        code[code_idx] = curr_button + '0';
        code_idx += 1;
    }

    try std.io.getStdOut().writer().print("Bathroom code: {s}\n", .{code[0..code_idx]});
}
