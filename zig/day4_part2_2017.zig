
const std = @import("std");

pub fn main() !void {
    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buf: [4096]u8 = undefined;
    const content = try file.readAll(&buf);
    var lines = std.mem.tokenize(u8, content, "\n");

    var valid: u32 = 0;
    while (lines.next()) |line| {
        var seen = std.StringHashMap(void).init(std.heap.page_allocator);
        defer seen.deinit();

        var words = std.mem.tokenize(u8, line, " \t");
        var ok = true;
        while (words.next()) |word| {
            var key: [32]u8 = undefined;
            const len = @min(word.len, key.len);
            std.mem.copy(u8, key[0..len], word[0..len]);
            std.mem.sort(u8, key[0..len], {}, std.sort.asc(u8));
            const k = key[0..len];
            if (seen.contains(k)) {
                ok = false;
                break;
            }
            try seen.put(k, {});
        }
        if (ok) valid += 1;
    }
    std.debug.print("{}\n", .{valid});
}
