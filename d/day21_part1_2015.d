
import std.stdio;
import std.file;
import std.conv;
import std.array;

struct Item {
    int cost;
    int damage;
    int armor;
}

struct Character {
    int hitPoints;
    int damage;
    int armor;
}

void main() {
    string data = cast(string)std.file.read("input.txt");
    string[] lines = data.split("\n");

    Character boss = Character(parseStat(lines[0]), parseStat(lines[1]), parseStat(lines[2]));

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

    int minCost = int.max;
    foreach (w; weapons) {
        foreach (a; armors) {
            foreach (ri; 0 .. rings.length) {
                foreach (rj; ri + 1 .. rings.length) {
                    Character player = Character(100, w.damage + rings[ri].damage + rings[rj].damage, a.armor + rings[ri].armor + rings[rj].armor);
                    int cost = w.cost + a.cost + rings[ri].cost + rings[rj].cost;
                    if (playerWins(player, boss) && cost < minCost) {
                        minCost = cost;
                    }
                }
            }
        }
    }

    writeln(minCost);
}

int parseStat(string line) {
    string[] parts = line.split(": ");
    return to!int(parts[1]);
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
