const std = @import("std");
const Md5 = std.crypto.hash.Md5;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Read input from file
    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    const input = try file.readToEndAlloc(allocator, 1024);
    defer allocator.free(input);

    // Remove trailing newline if present
    const secret_key = std.mem.trimRight(u8, input, "\r\n");

    var number: u64 = 1;
    while (true) : (number += 1) {
        var hash: [Md5.digest_length]u8 = undefined;
        var combined = try std.fmt.allocPrint(allocator, "{s}{d}", .{ secret_key, number });
        defer allocator.free(combined);

        Md5.hash(combined, &hash, .{});

        if (startsWithSixZeroes(&hash)) {
            break;
        }
    }

    // Print the result
    try std.io.getStdOut().writer().print("{d}\n", .{number});
}

fn startsWithSixZeroes(hash: *const [Md5.digest_length]u8) bool {
    return hash[0] == 0 and hash[1] == 0 and hash[2] == 0;
}
