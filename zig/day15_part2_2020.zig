
const std = @import("std");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(content);

    var numbers = std.ArrayList(u32).init(allocator);
    defer numbers.deinit();

    var it = std.mem.split(u8, std.mem.trim(u8, content, &std.ascii.whitespace), ",");
    while (it.next()) |num_str| {
        const num = try std.fmt.parseUnsigned(u32, num_str, 10);
        try numbers.append(num);
    }

    const target_turn: u32 = 30_000_000;
    var spoken = try allocator.alloc(u32, target_turn);
    defer allocator.free(spoken);
    @memset(spoken, 0);

    var last_spoken: u32 = 0;
    var turn: u32 = 1;

    for (numbers.items) |num| {
        if (turn == numbers.items.len) {
            last_spoken = num;
        } else {
            spoken[num] = turn;
        }
        turn += 1;
    }

    while (turn <= target_turn) : (turn += 1) {
        const next_number = if (spoken[last_spoken] != 0)
            turn - 1 - spoken[last_spoken]
        else
            0;
        spoken[last_spoken] = turn - 1;
        last_spoken = next_number;
    }

    const stdout = std.io.getStdOut().writer();
    try stdout.print("{}\n", .{last_spoken});
}
