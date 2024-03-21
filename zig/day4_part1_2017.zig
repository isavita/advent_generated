const std = @import("std");

pub fn main() !void {
    var file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buffer: [1024 * 1024]u8 = undefined;
    const bytes_read = try file.readAll(&buffer);
    const input = buffer[0..bytes_read];

    var passphrases = std.mem.split(u8, std.mem.trimRight(u8, input, "\n"), "\n");
    var validCount: usize = 0;

    while (passphrases.next()) |passphrase| {
        var words = std.mem.split(u8, std.mem.trimRight(u8, passphrase, " "), " ");
        var wordSet = std.StringHashMap(void).init(std.heap.page_allocator);
        defer wordSet.deinit();

        var valid = true;
        while (words.next()) |word| {
            if (wordSet.contains(word)) {
                valid = false;
                break;
            }
            try wordSet.put(word, {});
        }

        if (valid) {
            validCount += 1;
        }
    }

    std.debug.print("{}\n", .{validCount});
}