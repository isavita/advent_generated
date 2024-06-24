
const std = @import("std");
const Md5 = std.crypto.hash.Md5;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const input = try std.fs.cwd().readFileAlloc(allocator, "input.txt", 1024);
    defer allocator.free(input);

    const doorID = std.mem.trim(u8, input, &std.ascii.whitespace);
    const password = try findPassword(allocator, doorID);
    try std.io.getStdOut().writer().print("{s}\n", .{password});
}

fn findPassword(allocator: std.mem.Allocator, doorID: []const u8) ![]u8 {
    var password = try allocator.alloc(u8, 8);
    var found = [_]bool{false} ** 8;
    var filledPositions: usize = 0;
    var i: usize = 0;

    var buf: [64]u8 = undefined;
    var hash: [Md5.digest_length]u8 = undefined;

    while (filledPositions < 8) : (i += 1) {
        const input = try std.fmt.bufPrint(&buf, "{s}{d}", .{ doorID, i });
        Md5.hash(input, &hash, .{});

        if (hash[0] == 0 and hash[1] == 0 and hash[2] & 0xF0 == 0) {
            const pos = hash[2] & 0x0F;
            if (pos < 8 and !found[pos]) {
                found[pos] = true;
                password[pos] = std.fmt.digitToChar(@as(u4, @truncate(hash[3] >> 4)), .lower);
                filledPositions += 1;
            }
        }
    }

    return password;
}
