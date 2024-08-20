import std.stdio;
import std.file;
import std.conv;
import std.array;

struct Character {
    int hitPoints;
    int damage;
    int armor;
}

struct Item {
    int cost;
    int damage;
    int armor;
}

void main() {
    Character boss = readBossStats("input.txt");
    Character player = Character(100, 0, 0);

    Item[] weapons = [
        Item(8, 4, 0),
        Item(10, 5, 0),
        Item(25, 6, 0),
        Item(40, 7, 0),
        Item(74, 8, 0)
    ];

    Item[] armors = [
        Item(0, 0, 0),
        Item(13, 0, 1),
        Item(31, 0, 2),
        Item(53, 0, 3),
        Item(75, 0, 4),
        Item(102, 0, 5)
    ];

    Item[] rings = [
        Item(0, 0, 0),
        Item(25, 1, 0),
        Item(50, 2, 0),
        Item(100, 3, 0),
        Item(20, 0, 1),
        Item(40, 0, 2),
        Item(80, 0, 3)
    ];

    int minGoldToWin = int.max;
    int maxGoldToLose = 0;

    foreach (w; weapons) {
        foreach (a; armors) {
            foreach (r1; 0 .. rings.length) {
                foreach (r2; r1 + 1 .. rings.length) {
                    player.damage = w.damage + rings[r1].damage + rings[r2].damage;
                    player.armor = a.armor + rings[r1].armor + rings[r2].armor;

                    int cost = w.cost + a.cost + rings[r1].cost + rings[r2].cost;

                    if (playerWins(player, boss)) {
                        if (cost < minGoldToWin) {
                            minGoldToWin = cost;
                        }
                    } else {
                        if (cost > maxGoldToLose) {
                            maxGoldToLose = cost;
                        }
                    }
                }
            }
        }
    }

    writeln("Part 1: ", minGoldToWin);
    writeln("Part 2: ", maxGoldToLose);
}

Character readBossStats(string filename) {
    auto file = File(filename, "r");
    Character boss;

    foreach (line; file.byLine()) {
        auto parts = line.split(": ");
        if (parts[0] == "Hit Points") {
            boss.hitPoints = to!int(parts[1]);
        } else if (parts[0] == "Damage") {
            boss.damage = to!int(parts[1]);
        } else if (parts[0] == "Armor") {
            boss.armor = to!int(parts[1]);
        }
    }

    return boss;
}

bool playerWins(Character player, Character boss) {
    int playerDamage = max(1, player.damage - boss.armor);
    int bossDamage = max(1, boss.damage - player.armor);

    int playerTurns = (boss.hitPoints + playerDamage - 1) / playerDamage;
    int bossTurns = (player.hitPoints + bossDamage - 1) / bossDamage;

    return playerTurns <= bossTurns;
}

int max(int a, int b) {
    return a > b ? a : b;
}