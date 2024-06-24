
const std = @import("std");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    const input = try file.readToEndAlloc(allocator, 1024);
    defer allocator.free(input);

    var scoreboard = std.ArrayList(u8).init(allocator);
    defer scoreboard.deinit();
    try scoreboard.appendSlice(&[_]u8{ 3, 7 });

    var elf1: usize = 0;
    var elf2: usize = 1;

    while (true) {
        const new_score = scoreboard.items[elf1] + scoreboard.items[elf2];
        if (new_score >= 10) {
            try scoreboard.append(new_score / 10);
            if (checkSequence(scoreboard.items, input)) break;
        }
        try scoreboard.append(new_score % 10);
        if (checkSequence(scoreboard.items, input)) break;

        elf1 = (elf1 + scoreboard.items[elf1] + 1) % scoreboard.items.len;
        elf2 = (elf2 + scoreboard.items[elf2] + 1) % scoreboard.items.len;
    }

    const writer = std.io.getStdOut().writer();
    try writer.print("{d}\n", .{scoreboard.items.len - input.len});
}

fn checkSequence(scoreboard: []const u8, sequence: []const u8) bool {
    if (scoreboard.len < sequence.len) return false;
    const start = scoreboard.len - sequence.len;
    for (sequence, 0..) |v, i| {
        if (scoreboard[start + i] != v - '0') return false;
    }
    return true;
}
