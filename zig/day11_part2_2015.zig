const std = @import("std");

const PASSWORD_LENGTH = 8;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Read input from file
    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 1024);
    defer allocator.free(content);

    var password = try allocator.alloc(u8, PASSWORD_LENGTH);
    defer allocator.free(password);
    @memcpy(password, std.mem.trim(u8, content, &std.ascii.whitespace));

    // Find the first valid password
    findNextValidPassword(password);
    
    // Find the second valid password
    incrementPassword(password);
    findNextValidPassword(password);

    try std.io.getStdOut().writer().print("{s}\n", .{password});
}

fn findNextValidPassword(password: []u8) void {
    while (true) {
        incrementPassword(password);
        if (isValidPassword(password)) break;
    }
}

fn incrementPassword(password: []u8) void {
    var i: usize = PASSWORD_LENGTH;
    while (i > 0) {
        i -= 1;
        if (password[i] == 'z') {
            password[i] = 'a';
        } else {
            password[i] += 1;
            if (password[i] == 'i' or password[i] == 'o' or password[i] == 'l') {
                password[i] += 1;
            }
            break;
        }
    }
}

fn isValidPassword(password: []const u8) bool {
    return hasStraight(password) and !hasInvalidChars(password) and hasTwoPairs(password);
}

fn hasStraight(password: []const u8) bool {
    for (password[0 .. PASSWORD_LENGTH - 2], 0..) |c, i| {
        if (c + 1 == password[i + 1] and c + 2 == password[i + 2]) {
            return true;
        }
    }
    return false;
}

fn hasInvalidChars(password: []const u8) bool {
    for (password) |c| {
        if (c == 'i' or c == 'o' or c == 'l') {
            return true;
        }
    }
    return false;
}

fn hasTwoPairs(password: []const u8) bool {
    var first_pair: ?u8 = null;
    for (password[0 .. PASSWORD_LENGTH - 1], 0..) |c, i| {
        if (c == password[i + 1]) {
            if (first_pair) |fp| {
                if (fp != c) return true;
            } else {
                first_pair = c;
            }
        }
    }
    return false;
}
