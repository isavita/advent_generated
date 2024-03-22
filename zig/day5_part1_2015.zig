const std = @import("std");

fn isNice(s: []const u8) bool {
    var vowel_count: usize = 0;
    var double_letter: bool = false;
    var bad_substring: bool = false;

    var i: usize = 0;
    while (i < s.len) {
        if (std.mem.indexOfScalar(u8, "aeiou", s[i]) != null) {
            vowel_count += 1;
        }

        if (i + 1 < s.len and s[i] == s[i + 1]) {
            double_letter = true;
        }

        if (i + 1 < s.len and
            (std.mem.eql(u8, s[i .. i + 2], "ab") or
             std.mem.eql(u8, s[i .. i + 2], "cd") or
             std.mem.eql(u8, s[i .. i + 2], "pq") or
             std.mem.eql(u8, s[i .. i + 2], "xy")))
        {
            bad_substring = true;
            break;
        }

        i += 1;
    }

    return vowel_count >= 3 and double_letter and !bad_substring;
}

pub fn main() !void {
    var file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var reader = std.io.bufferedReader(file.reader());
    var line_reader = reader.reader();

    var nice_count: usize = 0;

    var buffer: [1024]u8 = undefined;
    while (try line_reader.readUntilDelimiterOrEof(&buffer, '\n')) |line| {
        const trimmed_line = std.mem.trim(u8, line, "\n");
        if (isNice(trimmed_line)) {
            nice_count += 1;
        }
    }

    std.debug.print("{d}\n", .{nice_count});
}