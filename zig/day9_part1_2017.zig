
const std = @import("std");
const fs = std.fs;
const io = std.io;

pub fn main() !void {
    // Open the input file
    var file = try fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buf_reader = io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var total_score: u32 = 0;
    var depth: u32 = 0;
    var in_garbage = false;
    var skip_next = false;

    while (in_stream.readByte()) |char| {
        if (skip_next) {
            skip_next = false;
            continue;
        }

        switch (char) {
            '{' => {
                if (!in_garbage) {
                    depth += 1;
                    total_score += depth;
                }
            },
            '}' => {
                if (!in_garbage) {
                    depth -= 1;
                }
            },
            '<' => {
                if (!in_garbage) {
                    in_garbage = true;
                }
            },
            '>' => {
                if (in_garbage) {
                    in_garbage = false;
                }
            },
            '!' => {
                skip_next = true;
            },
            else => {},
        }
    } else |err| {
        if (err != error.EndOfStream) {
            return err;
        }
    }

    // Print the result
    try std.io.getStdOut().writer().print("Total score: {}\n", .{total_score});
}
