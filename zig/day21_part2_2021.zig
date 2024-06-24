
const std = @import("std");

const Position = u8;
const Score = u8;
const Wins = u64;

const GameState = struct {
    positions: [2]Position,
    scores: [2]Score,
    rolls_left: u8,
    is_player1_turn: bool,
};

const MemoKey = struct {
    state: GameState,
    fn hash(self: @This()) u64 {
        return @as(u64, self.state.positions[0]) |
            (@as(u64, self.state.positions[1]) << 8) |
            (@as(u64, self.state.scores[0]) << 16) |
            (@as(u64, self.state.scores[1]) << 24) |
            (@as(u64, self.state.rolls_left) << 32) |
            (@as(u64, @intFromBool(self.state.is_player1_turn)) << 35);
    }
    fn eql(self: @This(), other: @This()) bool {
        return std.mem.eql(u8, std.mem.asBytes(&self.state), std.mem.asBytes(&other.state));
    }
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var buf: [1024]u8 = undefined;
    var positions: [2]Position = undefined;
    var i: usize = 0;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        positions[i] = try std.fmt.parseInt(Position, line[28..], 10);
        i += 1;
    }

    var memo = std.AutoHashMap(MemoKey, [2]Wins).init(allocator);
    defer memo.deinit();

    const result = try solve(positions, &memo);
    const stdout = std.io.getStdOut().writer();
    try stdout.print("{}\n", .{@max(result[0], result[1])});
}

fn solve(positions: [2]Position, memo: *std.AutoHashMap(MemoKey, [2]Wins)) ![2]Wins {
    const initial_state = GameState{
        .positions = positions,
        .scores = .{ 0, 0 },
        .rolls_left = 3,
        .is_player1_turn = true,
    };
    return play(initial_state, memo);
}

fn play(state: GameState, memo: *std.AutoHashMap(MemoKey, [2]Wins)) ![2]Wins {
    const key = MemoKey{ .state = state };
    if (memo.get(key)) |result| {
        return result;
    }

    var wins: [2]Wins = .{ 0, 0 };
    const player_index: usize = if (state.is_player1_turn) 0 else 1;

    if (state.rolls_left == 0) {
        var new_state = state;
        new_state.scores[player_index] += new_state.positions[player_index];

        if (new_state.scores[player_index] >= 21) {
            wins[player_index] = 1;
            try memo.put(key, wins);
            return wins;
        }

        new_state.is_player1_turn = !new_state.is_player1_turn;
        new_state.rolls_left = 3;
        return play(new_state, memo);
    }

    var roll: u8 = 1;
    while (roll <= 3) : (roll += 1) {
        var new_state = state;
        new_state.positions[player_index] += roll;
        if (new_state.positions[player_index] > 10) {
            new_state.positions[player_index] -= 10;
        }
        new_state.rolls_left -= 1;

        const result = try play(new_state, memo);
        wins[0] += result[0];
        wins[1] += result[1];
    }

    try memo.put(key, wins);
    return wins;
}
