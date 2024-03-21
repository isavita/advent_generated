const std = @import("std");

fn validatePassword(policy: []const u8, password: []const u8) bool {
    var min: usize = 0;
    var max: usize = 0;
    var char: u8 = 0;
    var count: usize = 0;

    var i: usize = 0;
    while (i < policy.len and policy[i] != '-') : (i += 1) {
        min = min * 10 + policy[i] - '0';
    }
    i += 1;
    while (i < policy.len and policy[i] != ' ') : (i += 1) {
        max = max * 10 + policy[i] - '0';
    }
    i += 1;
    char = policy[i];

    for (password) |c| {
        if (c == char) {
            count += 1;
        }
    }

    return count >= min and count <= max;
}

pub fn main() !void {
    var file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var reader = std.io.bufferedReader(file.reader());
    var line_buf: [1024]u8 = undefined;
    var line_stream = reader.reader();

    var validCount: usize = 0;
    while (try line_stream.readUntilDelimiterOrEof(&line_buf, '\n')) |line| {
        if (std.mem.indexOfScalar(u8, line, ':')) |i| {
            const policy = line[0..i];
            const password = line[i + 2 ..];
            if (validatePassword(policy, password)) {
                validCount += 1;
            }
        }
    }

    std.debug.print("{}\n", .{validCount});
}