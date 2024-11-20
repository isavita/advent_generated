
const std = @import("std");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var player1_deck = std.ArrayList(u8).init(allocator);
    var player2_deck = std.ArrayList(u8).init(allocator);
    defer player1_deck.deinit();
    defer player2_deck.deinit();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();
    var buf: [100]u8 = undefined;

    var current_deck = &player1_deck;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        if (line.len == 0) {
            current_deck = &player2_deck;
            continue;
        }
        if (std.mem.startsWith(u8, line, "Player")) continue;

        const card = try std.fmt.parseInt(u8, line, 10);
        try current_deck.append(card);
    }

    while (player1_deck.items.len > 0 and player2_deck.items.len > 0) {
        const card1 = player1_deck.items[0];
        const card2 = player2_deck.items[0];
        _ = player1_deck.orderedRemove(0);
        _ = player2_deck.orderedRemove(0);

        if (card1 > card2) {
            try player1_deck.append(card1);
            try player1_deck.append(card2);
        } else {
            try player2_deck.append(card2);
            try player2_deck.append(card1);
        }
    }

    const winning_deck = if (player1_deck.items.len > 0) player1_deck.items else player2_deck.items;

    var score: u32 = 0;
    for (winning_deck, 0..) |card, i| {
        score += card * @as(u32, @intCast(winning_deck.len - i));
    }

    const stdout = std.io.getStdOut().writer();
    try stdout.print("{}\n", .{score});
}
