const std = @import("std");
const Md5 = std.crypto.hash.Md5;

fn getMD5Hash(allocator: std.mem.Allocator, input: []const u8) ![]u8 {
    var hash: [Md5.digest_length]u8 = undefined;
    Md5.hash(input, &hash, .{});
    return try std.fmt.allocPrint(allocator, "{x}", .{std.fmt.fmtSliceHexLower(&hash)});
}

fn findTriplet(hash: []const u8) ?u8 {
    for (0..hash.len - 2) |i| {
        if (hash[i] == hash[i + 1] and hash[i] == hash[i + 2]) {
            return hash[i];
        }
    }
    return null;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    const salt = try file.readToEndAlloc(allocator, 1024);
    defer allocator.free(salt);

    var keys: usize = 0;
    var index: usize = 0;
    var cache = std.AutoHashMap(usize, []u8).init(allocator);
    defer {
        var it = cache.valueIterator();
        while (it.next()) |value| {
            allocator.free(value.*);
        }
        cache.deinit();
    }

    while (keys < 64) : (index += 1) {
        const input = try std.fmt.allocPrint(allocator, "{s}{d}", .{ salt, index });
        defer allocator.free(input);

        const hash = try getMD5Hash(allocator, input);
        defer allocator.free(hash);

        if (findTriplet(hash)) |triplet| {
            const target = [_]u8{triplet} ** 5;
            for (1..1001) |i| {
                const next_index = index + i;
                const next_hash = if (cache.get(next_index)) |h|
                    h
                else blk: {
                    const next_input = try std.fmt.allocPrint(allocator, "{s}{d}", .{ salt, next_index });
                    defer allocator.free(next_input);
                    const h = try getMD5Hash(allocator, next_input);
                    try cache.put(next_index, h);
                    break :blk h;
                };

                if (std.mem.indexOf(u8, next_hash, &target) != null) {
                    keys += 1;
                    break;
                }
            }
        }
    }

    const stdout = std.io.getStdOut().writer();
    try stdout.print("{d}\n", .{index - 1});
}