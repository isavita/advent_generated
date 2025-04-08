
#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <algorithm>
#include <limits>

using namespace std;

struct Item {
    int cost;
    int damage;
    int armor;
};

struct Character {
    int hit_points;
    int damage;
    int armor;
};

int parse_stat(const string& line) {
    size_t pos = line.find(": ");
    return stoi(line.substr(pos + 2));
}

bool player_wins(const Character& player, const Character& boss) {
    int player_damage = max(1, player.damage - boss.armor);
    int boss_damage = max(1, boss.damage - player.armor);

    int player_turns = (boss.hit_points + player_damage - 1) / player_damage;
    int boss_turns = (player.hit_points + boss_damage - 1) / boss_damage;

    return player_turns <= boss_turns;
}

int main() {
    ifstream data("input.txt");
    string line;
    vector<string> lines;
    while (getline(data, line)) {
        lines.push_back(line);
    }
    data.close();

    Character boss;
    boss.hit_points = parse_stat(lines[0]);
    boss.damage = parse_stat(lines[1]);
    boss.armor = parse_stat(lines[2]);

    Item weapons[] = {
        {8, 4, 0},
        {10, 5, 0},
        {25, 6, 0},
        {40, 7, 0},
        {74, 8, 0}
    };
    int num_weapons = sizeof(weapons) / sizeof(weapons[0]);

    Item armors[] = {
        {0, 0, 0},
        {13, 0, 1},
        {31, 0, 2},
        {53, 0, 3},
        {75, 0, 4},
        {102, 0, 5}
    };
    int num_armors = sizeof(armors) / sizeof(armors[0]);

    Item rings[] = {
        {0, 0, 0},
        {25, 1, 0},
        {50, 2, 0},
        {100, 3, 0},
        {20, 0, 1},
        {40, 0, 2},
        {80, 0, 3}
    };
    int num_rings = sizeof(rings) / sizeof(rings[0]);

    int max_cost = 0;

    for (int w = 0; w < num_weapons; ++w) {
        for (int a = 0; a < num_armors; ++a) {
            for (int ri = 0; ri < num_rings; ++ri) {
                for (int rj = ri + 1; rj < num_rings; ++rj) {
                    Character player = {100, weapons[w].damage + rings[ri].damage + rings[rj].damage, armors[a].armor + rings[ri].armor + rings[rj].armor};
                    int cost = weapons[w].cost + armors[a].cost + rings[ri].cost + rings[rj].cost;
                    if (!player_wins(player, boss) && cost > max_cost) {
                        max_cost = cost;
                    }
                }
            }
        }
    }

    cout << max_cost << endl;

    return 0;
}
