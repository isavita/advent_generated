const std = @import("std");

const Item = struct {
    cost: u32,
    damage: u32,
    armor: u32,
};

const Character = struct {
    hit_points: u32,
    damage: u32,
    armor: u32,
};

const weapons = [_]Item{
    .{ .cost = 8, .damage = 4, .armor = 0 },
    .{ .cost = 10, .damage = 5, .armor = 0 },
    .{ .cost = 25, .damage = 6, .armor = 0 },
    .{ .cost = 40, .damage = 7, .armor = 0 },
    .{ .cost = 74, .damage = 8, .armor = 0 },
};

const armors = [_]Item{
    .{ .cost = 0, .damage = 0, .armor = 0 }, // No armor
    .{ .cost = 13, .damage = 0, .armor = 1 },
    .{ .cost = 31, .damage = 0, .armor = 2 },
    .{ .cost = 53, .damage = 0, .armor = 3 },
    .{ .cost = 75, .damage = 0, .armor = 4 },
    .{ .cost = 102, .damage = 0, .armor = 5 },
};

const rings = [_]Item{
    .{ .cost = 0, .damage = 0, .armor = 0 }, // No ring
    .{ .cost = 25, .damage = 1, .armor = 0 },
    .{ .cost = 50, .damage = 2, .armor = 0 },
    .{ .cost = 100, .damage = 3, .armor = 0 },
    .{ .cost = 20, .damage = 0, .armor = 1 },
    .{ .cost = 40, .damage = 0, .armor = 2 },
    .{ .cost = 80, .damage = 0, .armor = 3 },
};

fn simulateBattle(player: Character, boss: Character) bool {
    var player_hp = player.hit_points;
    var boss_hp = boss.hit_points;

    while (true) {
        boss_hp -|= @max(1, player.damage -| boss.armor);
        if (boss_hp == 0) return true;

        player_hp -|= @max(1, boss.damage -| player.armor);
        if (player_hp == 0) return false;
    }
}

fn findMostExpensiveLosingEquipment(boss: Character) u32 {
    var max_cost: u32 = 0;

    for (weapons) |weapon| {
        for (armors) |armor| {
            for (rings, 0..) |ring1, i| {
                for (rings[i + 1 ..]) |ring2| {
                    const cost = weapon.cost + armor.cost + ring1.cost + ring2.cost;
                    if (cost <= max_cost) continue;

                    const player = Character{
                        .hit_points = 100,
                        .damage = weapon.damage + ring1.damage + ring2.damage,
                        .armor = armor.armor + ring1.armor + ring2.armor,
                    };

                    if (!simulateBattle(player, boss)) {
                        max_cost = cost;
                    }
                }
            }
        }
    }

    return max_cost;
}

pub fn main() !void {
    // Read boss stats from input file
    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var buf: [100]u8 = undefined;
    var boss = Character{ .hit_points = 0, .damage = 0, .armor = 0 };

    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var parts = std.mem.split(u8, line, ": ");
        const key = parts.next().?;
        const value = try std.fmt.parseInt(u32, parts.next().?, 10);

        if (std.mem.eql(u8, key, "Hit Points")) {
            boss.hit_points = value;
        } else if (std.mem.eql(u8, key, "Damage")) {
            boss.damage = value;
        } else if (std.mem.eql(u8, key, "Armor")) {
            boss.armor = value;
        }
    }

    const max_cost = findMostExpensiveLosingEquipment(boss);

    const stdout = std.io.getStdOut().writer();
    try stdout.print("Most amount of gold to spend and still lose: {}\n", .{max_cost});
}
