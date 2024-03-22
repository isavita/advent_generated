const std = @import("std");

pub fn main() !void {
    // Step 1: Read Input
    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();
    const reader = file.reader();

    // Step 2: Initialize Variables
    var score: i32 = 0;
    var depth: i32 = 0;
    var in_garbage: bool = false;
    var cancel_next: bool = false;
    var garbage_count: i32 = 0;

    // Step 3: Process Stream
    var buf: [1024]u8 = undefined;
    while (true) {
        const bytes_read = try reader.read(&buf);
        if (bytes_read == 0) break;
        for (buf[0..bytes_read]) |ch| {
            if (cancel_next) {
                cancel_next = false;
                continue;
            }

            if (in_garbage) {
                if (ch == '!') {
                    cancel_next = true;
                } else if (ch == '>') {
                    in_garbage = false;
                } else {
                    garbage_count += 1;
                }
            } else {
                switch (ch) {
                    '{' => depth += 1,
                    '}' => {
                        score += depth;
                        depth -= 1;
                    },
                    '<' => in_garbage = true,
                    else => {},
                }
            }
        }
    }

    // Step 4: Print Results
    std.debug.print("{}\n", .{garbage_count});
}