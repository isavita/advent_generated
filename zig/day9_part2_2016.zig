
const std = @import("std");
const fs = std.fs;
const io = std.io;
const mem = std.mem;

fn decompressedLength(input: []const u8, recursive: bool) !u64 {
    var length: u64 = 0;
    var i: usize = 0;

    while (i < input.len) {
        if (input[i] == '(') {
            var j = i + 1;
            while (j < input.len and input[j] != ')') : (j += 1) {}
            const marker = input[i + 1 .. j];
            var parts = mem.split(u8, marker, "x");
            const count = try std.fmt.parseInt(usize, parts.next().?, 10);
            const repeat = try std.fmt.parseInt(usize, parts.next().?, 10);

            if (recursive) {
                const subseq = input[j + 1 .. j + 1 + count];
                length += repeat * try decompressedLength(subseq, true);
            } else {
                length += count * repeat;
            }

            i = j + 1 + count;
        } else {
            length += 1;
            i += 1;
        }
    }

    return length;
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const file = try fs.cwd().openFile("input.txt", .{});
    defer file.close();

    const input = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(input);

    const part1 = try decompressedLength(input, false);
    const part2 = try decompressedLength(input, true);

    const stdout = io.getStdOut().writer();
    try stdout.print("Part 1: {}\n", .{part1});
    try stdout.print("Part 2: {}\n", .{part2});
}
