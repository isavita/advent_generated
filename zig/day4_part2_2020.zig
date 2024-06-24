
const std = @import("std");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(content);

    var valid_passports: usize = 0;
    var it = std.mem.split(u8, content, "\n\n");
    while (it.next()) |passport| {
        if (isValidPassport(passport)) valid_passports += 1;
    }

    const stdout = std.io.getStdOut().writer();
    try stdout.print("{d}\n", .{valid_passports});
}

fn isValidPassport(passport: []const u8) bool {
    const required_fields = [_][]const u8{ "byr:", "iyr:", "eyr:", "hgt:", "hcl:", "ecl:", "pid:" };
    var field_count: u8 = 0;

    var it = std.mem.tokenize(u8, passport, " \n");
    while (it.next()) |field| {
        for (required_fields) |req_field| {
            if (std.mem.startsWith(u8, field, req_field)) {
                field_count += 1;
                const value = field[4..];
                switch (req_field[0]) {
                    'b' => if (!validateYear(value, 1920, 2002)) return false,
                    'i' => if (!validateYear(value, 2010, 2020)) return false,
                    'e' => if (req_field[1] == 'y') {
                        if (!validateYear(value, 2020, 2030)) return false;
                    } else {
                        if (!validateEcl(value)) return false;
                    },
                    'h' => if (req_field[1] == 'g') {
                        if (!validateHgt(value)) return false;
                    } else {
                        if (!validateHcl(value)) return false;
                    },
                    'p' => if (!validatePid(value)) return false,
                    else => {},
                }
                break;
            }
        }
    }

    return field_count == required_fields.len;
}

fn validateYear(value: []const u8, min: u16, max: u16) bool {
    const year = std.fmt.parseInt(u16, value, 10) catch return false;
    return year >= min and year <= max;
}

fn validateHgt(value: []const u8) bool {
    if (value.len < 3) return false;
    const num = std.fmt.parseInt(u16, value[0 .. value.len - 2], 10) catch return false;
    return switch (value[value.len - 2]) {
        'c' => num >= 150 and num <= 193,
        'i' => num >= 59 and num <= 76,
        else => false,
    };
}

fn validateHcl(value: []const u8) bool {
    if (value.len != 7 or value[0] != '#') return false;
    for (value[1..]) |c| {
        switch (c) {
            '0'...'9', 'a'...'f' => {},
            else => return false,
        }
    }
    return true;
}

fn validateEcl(value: []const u8) bool {
    const valid_ecl = [_][]const u8{ "amb", "blu", "brn", "gry", "grn", "hzl", "oth" };
    for (valid_ecl) |ecl| {
        if (std.mem.eql(u8, value, ecl)) return true;
    }
    return false;
}

fn validatePid(value: []const u8) bool {
    if (value.len != 9) return false;
    for (value) |c| {
        switch (c) {
            '0'...'9' => {},
            else => return false,
        }
    }
    return true;
}
