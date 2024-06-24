
const std = @import("std");

const GameState = struct {
    player_hp: i32,
    player_mana: i32,
    boss_hp: i32,
    boss_damage: i32,
    shield_timer: i32,
    poison_timer: i32,
    recharge_timer: i32,
    mana_spent: i32,
};

fn minManaToWin(initial_state: GameState) i32 {
    var min_mana: i32 = std.math.maxInt(i32);

    const simulate = struct {
        fn f(state: GameState, player_turn: bool, min_mana_ptr: *i32) void {
            if (state.mana_spent >= min_mana_ptr.*) return;
            if (state.boss_hp <= 0) {
                min_mana_ptr.* = state.mana_spent;
                return;
            }
            if (state.player_hp <= 0) return;

            var new_state = state;

            if (new_state.shield_timer > 0) new_state.shield_timer -= 1;
            if (new_state.poison_timer > 0) {
                new_state.boss_hp -= 3;
                new_state.poison_timer -= 1;
            }
            if (new_state.recharge_timer > 0) {
                new_state.player_mana += 101;
                new_state.recharge_timer -= 1;
            }

            if (!player_turn) {
                var damage = new_state.boss_damage;
                if (new_state.shield_timer > 0) damage -= 7;
                if (damage < 1) damage = 1;
                new_state.player_hp -= damage;
                f(new_state, true, min_mana_ptr);
                return;
            }

            if (new_state.player_mana >= 53) {
                var spell_state = new_state;
                spell_state.player_mana -= 53;
                spell_state.mana_spent += 53;
                spell_state.boss_hp -= 4;
                f(spell_state, false, min_mana_ptr);
            }
            if (new_state.player_mana >= 73) {
                var spell_state = new_state;
                spell_state.player_mana -= 73;
                spell_state.mana_spent += 73;
                spell_state.boss_hp -= 2;
                spell_state.player_hp += 2;
                f(spell_state, false, min_mana_ptr);
            }
            if (new_state.player_mana >= 113 and new_state.shield_timer == 0) {
                var spell_state = new_state;
                spell_state.player_mana -= 113;
                spell_state.mana_spent += 113;
                spell_state.shield_timer = 6;
                f(spell_state, false, min_mana_ptr);
            }
            if (new_state.player_mana >= 173 and new_state.poison_timer == 0) {
                var spell_state = new_state;
                spell_state.player_mana -= 173;
                spell_state.mana_spent += 173;
                spell_state.poison_timer = 6;
                f(spell_state, false, min_mana_ptr);
            }
            if (new_state.player_mana >= 229 and new_state.recharge_timer == 0) {
                var spell_state = new_state;
                spell_state.player_mana -= 229;
                spell_state.mana_spent += 229;
                spell_state.recharge_timer = 5;
                f(spell_state, false, min_mana_ptr);
            }
        }
    }.f;

    var initial = initial_state;
    initial.player_hp = 50;
    initial.player_mana = 500;
    simulate(initial, true, &min_mana);
    return min_mana;
}

pub fn main() !void {
    var file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var buf: [1024]u8 = undefined;
    var boss_hp: i32 = 0;
    var boss_damage: i32 = 0;

    if (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        boss_hp = try std.fmt.parseInt(i32, line[line.len - 2..], 10);
    }
    if (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        boss_damage = try std.fmt.parseInt(i32, line[line.len - 1..], 10);
    }

    const initial_state = GameState{
        .player_hp = 0,
        .player_mana = 0,
        .boss_hp = boss_hp,
        .boss_damage = boss_damage,
        .shield_timer = 0,
        .poison_timer = 0,
        .recharge_timer = 0,
        .mana_spent = 0,
    };

    const stdout = std.io.getStdOut().writer();
    try stdout.print("{d}\n", .{minManaToWin(initial_state)});
}
