
const std = @import("std");
const Md5 = std.crypto.hash.Md5;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Read input from file
    const input = try std.fs.cwd().readFileAlloc(allocator, "input.txt", 1024);
    defer allocator.free(input);

    const door_id = std.mem.trim(u8, input, "\r\n");

    var password: [8]u8 = undefined;
    var found: usize = 0;
    var index: usize = 0;

    while (found < 8) {
        var hash: [Md5.digest_length]u8 = undefined;
        var input_buffer: [64]u8 = undefined;

        const formatted = try std.fmt.bufPrint(&input_buffer, "{s}{d}", .{ door_id, index });
        Md5.hash(formatted, &hash, .{});

        if (hash[0] == 0 and hash[1] == 0 and (hash[2] & 0xF0) == 0) {
            password[found] = std.fmt.digitToChar(@as(u4, @truncate(hash[2])), .lower);
            found += 1;
            std.debug.print("Found character {d}: {c}\n", .{ found, password[found - 1] });
        }

        index += 1;
    }

    const stdout = std.io.getStdOut().writer();
    try stdout.print("Password: {s}\n", .{password});
}
