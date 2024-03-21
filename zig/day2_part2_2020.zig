const std = @import("std");

fn validatePassword(policy: []const u8, password: []const u8) bool {
    var min: usize = 0;
    var max: usize = 0;
    var char: u8 = 0;
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
    return (password[min - 1] == char) != (password[max - 1] == char);
}

pub fn main() !void {
    var file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var reader = std.io.bufferedReader(file.reader());
    var buffer: [1024]u8 = undefined;
    var validCount: usize = 0;

    while (try reader.reader().readUntilDelimiterOrEof(&buffer, '\n')) |line| {
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