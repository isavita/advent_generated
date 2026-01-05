
const std = @import("std");

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    const input = try std.fs.cwd().readFileAlloc(allocator, "input.txt", 1_000_000);
    defer allocator.free(input);

    var data = try std.ArrayList(u8).initCapacity(allocator, 1_000_000);
    defer data.deinit();
    try data.appendSlice(input);

    while (data.items.len < 272) {
        const old_len = data.items.len;
        try data.append('0');
        try data.ensureUnusedCapacity(old_len);
        for (data.items[0..old_len]) |c| {
            data.appendAssumeCapacity(if (c == '0') '1' else '0');
        }
        std.mem.reverse(u8, data.items[old_len + 1 ..]);
    }
    data.shrinkAndFree(272);

    while (data.items.len % 2 == 0) {
        const new_len = data.items.len / 2;
        for (0..new_len) |i| {
            const a = data.items[2 * i];
            const b = data.items[2 * i + 1];
            data.items[i] = if (a == b) '1' else '0';
        }
        data.shrinkAndFree(new_len);
    }

    const stdout = std.io.getStdOut().writer();
    try stdout.print("{s}\n", .{data.items});
}
