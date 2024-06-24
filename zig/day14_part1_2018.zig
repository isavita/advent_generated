
const std = @import("std");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    const input = try readInput(file);

    var scoreboard = try std.ArrayList(u8).initCapacity(allocator, input + 10);
    defer scoreboard.deinit();

    scoreboard.appendSlice(&[_]u8{ 3, 7 }) catch unreachable;

    var elf1: usize = 0;
    var elf2: usize = 1;

    while (scoreboard.items.len < input + 10) {
        const new_score = scoreboard.items[elf1] + scoreboard.items[elf2];
        if (new_score >= 10) {
            scoreboard.append(new_score / 10) catch unreachable;
        }
        scoreboard.append(new_score % 10) catch unreachable;

        elf1 = (elf1 + scoreboard.items[elf1] + 1) % scoreboard.items.len;
        elf2 = (elf2 + scoreboard.items[elf2] + 1) % scoreboard.items.len;
    }

    const stdout = std.io.getStdOut().writer();
    for (scoreboard.items[input..input+10]) |score| {
        try stdout.print("{d}", .{score});
    }
    try stdout.print("\n", .{});
}

fn readInput(file: std.fs.File) !usize {
    var buf: [20]u8 = undefined;
    const bytes_read = try file.readAll(&buf);
    return try std.fmt.parseInt(usize, buf[0..bytes_read], 10);
}
