const std = @import("std");

pub fn main() !void {
    var file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var group_answers: [26]bool = undefined;
    var total: usize = 0;
    var group_total: usize = 0;

    while (true) {
        var buf: [1024]u8 = undefined;
        const line = try in_stream.readUntilDelimiterOrEof(&buf, '\n');
        if (line) |l| {
            const trimmed = std.mem.trimRight(u8, l, "\r\n");
            if (trimmed.len == 0) {
                total += group_total;
                group_total = 0;
                @memset(&group_answers, false);
            } else {
                for (trimmed) |c| {
                    const index = c - 'a';
                    if (!group_answers[index]) {
                        group_answers[index] = true;
                        group_total += 1;
                    }
                }
            }
        } else {
            total += group_total;
            break;
        }
    }

    std.debug.print("{}\n", .{total});
}