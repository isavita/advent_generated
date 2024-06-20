const std = @import("std");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Read input from file
    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(content);

    var nice_strings_part1: usize = 0;
    var nice_strings_part2: usize = 0;

    var lines = std.mem.split(u8, content, "\n");
    while (lines.next()) |line| {
        if (line.len == 0) continue;
        if (isNiceStringPart1(line)) nice_strings_part1 += 1;
        if (isNiceStringPart2(line)) nice_strings_part2 += 1;
    }

    // Print results
    try std.io.getStdOut().writer().print("Part 1: {d}\n", .{nice_strings_part1});
    try std.io.getStdOut().writer().print("Part 2: {d}\n", .{nice_strings_part2});
}

fn isNiceStringPart1(s: []const u8) bool {
    var vowel_count: usize = 0;
    var has_double = false;
    var i: usize = 0;
    while (i < s.len) : (i += 1) {
        if (std.mem.indexOfScalar(u8, "aeiou", s[i]) != null) {
            vowel_count += 1;
        }
        if (i > 0) {
            if (s[i] == s[i-1]) has_double = true;
            if (std.mem.eql(u8, s[i-1..i+1], "ab") or
                std.mem.eql(u8, s[i-1..i+1], "cd") or
                std.mem.eql(u8, s[i-1..i+1], "pq") or
                std.mem.eql(u8, s[i-1..i+1], "xy")) {
                return false;
            }
        }
    }
    return vowel_count >= 3 and has_double;
}

fn isNiceStringPart2(s: []const u8) bool {
    var has_pair = false;
    var has_repeat = false;

    var i: usize = 0;
    while (i < s.len - 1) : (i += 1) {
        if (!has_pair and i < s.len - 3) {
            const pair = s[i..i+2];
            if (std.mem.indexOf(u8, s[i+2..], pair) != null) {
                has_pair = true;
            }
        }
        if (!has_repeat and i < s.len - 2) {
            if (s[i] == s[i+2]) {
                has_repeat = true;
            }
        }
        if (has_pair and has_repeat) return true;
    }

    return false;
}
