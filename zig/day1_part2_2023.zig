
const std = @import("std");

pub fn main() !void {
    var file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var sum: u32 = 0;
    var buf: [1024]u8 = undefined;

    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        const first_digit = findFirstDigit(line);
        const last_digit = findLastDigit(line);
        sum += first_digit * 10 + last_digit;
    }

    const stdout = std.io.getStdOut().writer();
    try stdout.print("{d}\n", .{sum});
}

fn findFirstDigit(line: []const u8) u8 {
    const digits = [_][]const u8{ "zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine" };
    var i: usize = 0;
    while (i < line.len) : (i += 1) {
        if (std.ascii.isDigit(line[i])) {
            return line[i] - '0';
        }
        for (digits, 0..) |digit, j| {
            if (std.mem.startsWith(u8, line[i..], digit)) {
                return @intCast(j);
            }
        }
    }
    unreachable;
}

fn findLastDigit(line: []const u8) u8 {
    const digits = [_][]const u8{ "zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine" };
    var i: usize = line.len;
    while (i > 0) : (i -= 1) {
        if (std.ascii.isDigit(line[i - 1])) {
            return line[i - 1] - '0';
        }
        for (digits, 0..) |digit, j| {
            if (i >= digit.len and std.mem.eql(u8, line[i - digit.len .. i], digit)) {
                return @intCast(j);
            }
        }
    }
    unreachable;
}
