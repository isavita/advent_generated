const std = @import("std");
const ArrayList = std.ArrayList;
const mem = std.mem;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var passports = ArrayList([]const u8).init(allocator);
    defer passports.deinit();

    var buf: [1024]u8 = undefined;
    var passport = ArrayList(u8).init(allocator);
    defer passport.deinit();

    while (try file.reader().readUntilDelimiterOrEof(&buf, '\n')) |line| {
        if (line.len == 0) {
            try passports.append(try passport.toOwnedSlice());
        } else {
            try passport.appendSlice(line);
            try passport.append(' ');
        }
    }
    if (passport.items.len > 0) {
        try passports.append(try passport.toOwnedSlice());
    }

    const required_fields = [_][]const u8{ "byr:", "iyr:", "eyr:", "hgt:", "hcl:", "ecl:", "pid:" };
    var valid_passports: usize = 0;

    for (passports.items) |p| {
        if (isValid(p, &required_fields)) {
            valid_passports += 1;
        }
    }

    const stdout = std.io.getStdOut().writer();
    try stdout.print("{d}\n", .{valid_passports});
}

fn isValid(passport: []const u8, required_fields: []const []const u8) bool {
    for (required_fields) |field| {
        if (mem.indexOf(u8, passport, field) == null) {
            return false;
        }
    }
    return true;
}