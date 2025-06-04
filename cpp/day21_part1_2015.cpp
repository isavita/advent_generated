
#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <limits>
#include <fstream>

struct Item {
    int cost;
    int damage;
    int armor;
};

struct CharacterStats {
    int hp;
    int damage;
    int armor;
};

int main() {
    CharacterStats boss_stats;
    std::ifstream inputFile("input.txt");
    std::string line;

    std::getline(inputFile, line);
    boss_stats.hp = std::stoi(line.substr(line.find(':') + 2));
    std::getline(inputFile, line);
    boss_stats.damage = std::stoi(line.substr(line.find(':') + 2));
    std::getline(inputFile, line);
    boss_stats.armor = std::stoi(line.substr(line.find(':') + 2));

    inputFile.close();

    std::vector<Item> weapons = {
        {8, 4, 0},
        {10, 5, 0},
        {25, 6, 0},
        {40, 7, 0},
        {74, 8, 0}
    };

    std::vector<Item> armor = {
        {0, 0, 0},
        {13, 0, 1},
        {31, 0, 2},
        {53, 0, 3},
        {75, 0, 4},
        {102, 0, 5}
    };

    std::vector<Item> rings = {
        {0, 0, 0},
        {25, 1, 0},
        {50, 2, 0},
        {100, 3, 0},
        {20, 0, 1},
        {40, 0, 2},
        {80, 0, 3}
    };

    CharacterStats player_base_stats = {100, 0, 0};

    int min_gold = std::numeric_limits<int>::max();

    for (const auto& w : weapons) {
        for (const auto& a : armor) {
            for (size_t r1_idx = 0; r1_idx < rings.size(); ++r1_idx) {
                for (size_t r2_idx = r1_idx + 1; r2_idx < rings.size(); ++r2_idx) {
                    const auto& r1 = rings[r1_idx];
                    const auto& r2 = rings[r2_idx];

                    int current_cost = w.cost + a.cost + r1.cost + r2.cost;
                    int player_current_damage = w.damage + a.damage + r1.damage + r2.damage;
                    int player_current_armor = w.armor + a.armor + r1.armor + r2.armor;

                    int player_effective_damage = std::max(1, player_current_damage - boss_stats.armor);
                    int player_turns = (boss_stats.hp + player_effective_damage - 1) / player_effective_damage;

                    int boss_effective_damage = std::max(1, boss_stats.damage - player_current_armor);
                    int boss_turns = (player_base_stats.hp + boss_effective_damage - 1) / boss_effective_damage;

                    if (player_turns <= boss_turns) {
                        min_gold = std::min(min_gold, current_cost);
                    }
                }
            }
        }
    }

    std::cout << min_gold << std::endl;

    return 0;
}
