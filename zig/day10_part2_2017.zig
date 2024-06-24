
const std = @import("std");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const input = try std.fs.cwd().readFileAlloc(allocator, "input.txt", 1024 * 1024);
    defer allocator.free(input);

    var lengths = try allocator.alloc(u8, input.len + 5);
    defer allocator.free(lengths);
    @memcpy(lengths[0..input.len], input);
    lengths[input.len..][0..5].* = .{ 17, 31, 73, 47, 23 };

    var list: [256]u8 = undefined;
    for (0..256) |i| list[i] = @intCast(i);

    var current_position: usize = 0;
    var skip_size: usize = 0;

    for (0..64) |_| {
        for (lengths) |length| {
            var i: usize = 0;
            while (i < length / 2) : (i += 1) {
                const start = (current_position + i) % 256;
                const end = (current_position + length - 1 - i) % 256;
                std.mem.swap(u8, &list[start], &list[end]);
            }
            current_position = (current_position + length + skip_size) % 256;
            skip_size += 1;
        }
    }

    var dense_hash: [16]u8 = undefined;
    for (0..16) |i| {
        var xor: u8 = 0;
        for (0..16) |j| {
            xor ^= list[i * 16 + j];
        }
        dense_hash[i] = xor;
    }

    var hex_hash: [32]u8 = undefined;
    _ = try std.fmt.bufPrint(&hex_hash, "{s}", .{std.fmt.fmtSliceHexLower(&dense_hash)});

    try std.io.getStdOut().writer().print("{s}\n", .{hex_hash});
}
