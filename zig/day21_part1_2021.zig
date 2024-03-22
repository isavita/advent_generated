const std = @import("std");

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    const contents = try file.readToEndAlloc(allocator, std.math.maxInt(usize));
    defer allocator.free(contents);

    var lines = std.mem.split(u8, contents, "\n");

    const player1StartStr = lines.next().?;
    const player1StartPos = try std.fmt.parseInt(i32, player1StartStr[28..], 10);
    const player2StartStr = lines.next().?;
    const player2StartPos = try std.fmt.parseInt(i32, player2StartStr[28..], 10);

    var player1Pos: i32 = player1StartPos;
    var player2Pos: i32 = player2StartPos;

    var player1Score: i32 = 0;
    var player2Score: i32 = 0;

    var dieRoll: i32 = 1;
    var rollCount: i32 = 0;

    while (true) {
        // Player 1
        const rolls = @mod(dieRoll, 100) + @mod(dieRoll + 1, 100) + @mod(dieRoll + 2, 100);
        rollCount += 3;
        dieRoll += 3;

        player1Pos = @mod(player1Pos + rolls - 1, 10) + 1;
        player1Score += player1Pos;

        if (player1Score >= 1000) {
            std.debug.print("Result: {}\n", .{player2Score * rollCount});
            return;
        }

        // Player 2
        const rolls2 = @mod(dieRoll, 100) + @mod(dieRoll + 1, 100) + @mod(dieRoll + 2, 100);
        rollCount += 3;
        dieRoll += 3;

        player2Pos = @mod(player2Pos + rolls2 - 1, 10) + 1;
        player2Score += player2Pos;

        if (player2Score >= 1000) {
            std.debug.print("{}\n", .{player1Score * rollCount});
            return;
        }
    }
}