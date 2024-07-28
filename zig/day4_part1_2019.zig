const std = @import("std");

fn hasDoubleDigits(num: u32) bool {
    var n = num;
    var last_digit: u32 = 10;
    var has_double = false;

    while (n > 0) {
        const digit = n % 10;
        if (digit > last_digit) {
            return false;
        }
        if (digit == last_digit) {
            has_double = true;
        }
        last_digit = digit;
        n /= 10;
    }

    return has_double;
}

fn countValidPasswords(start: u32, end: u32) u32 {
    var count: u32 = 0;
    var num: u32 = start;

    while (num <= end) {
        if (hasDoubleDigits(num)) {
            count += 1;
        }
        num += 1;
    }

    return count;
}

pub fn main() !void {
    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var buf: [100]u8 = undefined;
    const line = try in_stream.readUntilDelimiterOrEof(&buf, '\n');
    
    if (line) |l| {
        var it = std.mem.split(u8, l, "-");
        const start = try std.fmt.parseInt(u32, it.next().?, 10);
        const end = try std.fmt.parseInt(u32, it.next().?, 10);

        const valid_passwords = countValidPasswords(start, end);

        const stdout = std.io.getStdOut().writer();
        try stdout.print("Valid passwords count: {}\n", .{valid_passwords});
    } else {
        std.debug.print("Error: Input file is empty\n", .{});
    }
}