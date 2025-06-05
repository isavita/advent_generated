
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <map>
#include <queue>
#include <algorithm>
#include <limits>
#include <memory>

enum Kind {
    KIND_SPACE = 1,
    KIND_ELF = 2,
    KIND_GOBLIN = 4,
    KIND_WALL = 8,
    KIND_HIGHLIGHT = 16,
};

const int DEFAULT_HITPOINTS = 200;
const int DEFAULT_POWER = 3;

const std::vector<std::pair<int, int>> OFFSETS = {
    {0, -1}, // Up
    {-1, 0}, // Left
    {1, 0},  // Right
    {0, 1},  // Down
};

std::map<char, Kind> RUNE_KINDS = {
    {'.', KIND_SPACE},
    {'E', KIND_ELF},
    {'G', KIND_GOBLIN},
    {'#', KIND_WALL},
};

inline bool is_unit_kind(Kind k) {
    return (k == KIND_ELF || k == KIND_GOBLIN);
}

class Unit;
class Tile;
class Cave;

class Tile {
public:
    Kind kind;
    int x, y;
    Unit* unit;

    Tile(Kind k, int _x, int _y) : kind(k), x(_x), y(_y), unit(nullptr) {}
    Tile() : kind(KIND_SPACE), x(-1), y(-1), unit(nullptr) {}
};

class Unit {
public:
    Kind kind;
    int hitpoints;
    int power;
    Tile* tile;

    Unit(Tile* t, Kind k, int elfPower);

    bool targets(const Cave* c) const;
    std::vector<Unit*> enemies(const Cave* c) const;
    Unit* enemy_neighbor(const Cave* c) const;
    void move(Cave* c);
    bool attack(Cave* c);
    bool damage(Cave* c, int damage);
};

class Cave {
public:
    std::vector<std::vector<Tile>> map;
    std::vector<std::shared_ptr<Unit>> units;

    Cave(const std::vector<std::string>& input, int elf_power);

    Tile* get_tile(int y, int x) const {
        if (y >= 0 && y < map.size() && x >= 0 && x < map[0].size()) {
            return const_cast<Tile*>(&map[y][x]);
        }
        return nullptr;
    }

    std::vector<Tile*> get_walkable_neighbors(Tile* t) const {
        std::vector<Tile*> neighbors;
        for (const auto& offset : OFFSETS) {
            Tile* n = get_tile(t->y + offset.second, t->x + offset.first);
            if (n && n->kind == KIND_SPACE) {
                neighbors.push_back(n);
            }
        }
        return neighbors;
    }

    void parse_map(const std::vector<std::string>& input, int elf_power);
    std::pair<int, bool> status() const;
    void remove_the_dead();
    void remove_unit(Unit* u);
    std::pair<bool, bool> tick(bool stop_on_elf_death);
};

std::pair<std::map<Tile*, int>, std::map<Tile*, Tile*>> find_walkable_tiles(const Cave* c, Tile* t);

int combat(const std::vector<std::string>& input);
int cheating_elves(const std::vector<std::string>& input);

Unit::Unit(Tile* t, Kind k, int elfPower) : tile(t), kind(k), hitpoints(DEFAULT_HITPOINTS), power(DEFAULT_POWER) {
    if (kind == KIND_ELF) {
        this->power = elfPower;
    }
    if (tile) {
        tile->unit = this;
    }
}

bool Unit::targets(const Cave* c) const {
    for (const auto& u_ptr : c->units) {
        if (u_ptr->hitpoints > 0 && u_ptr->kind != this->kind) {
            return true;
        }
    }
    return false;
}

std::vector<Unit*> Unit::enemies(const Cave* c) const {
    std::vector<Unit*> enemy_units;
    for (const auto& u_ptr : c->units) {
        if (u_ptr->hitpoints > 0 && u_ptr->kind != this->kind) {
            enemy_units.push_back(u_ptr.get());
        }
    }
    std::sort(enemy_units.begin(), enemy_units.end(), [](Unit* a, Unit* b) {
        if (a->tile->y != b->tile->y) {
            return a->tile->y < b->tile->y;
        }
        return a->tile->x < b->tile->x;
    });
    return enemy_units;
}

Unit* Unit::enemy_neighbor(const Cave* c) const {
    Unit* target = nullptr;
    for (const auto& offset : OFFSETS) {
        Tile* t = c->get_tile(this->tile->y + offset.second, this->tile->x + offset.first);
        if (t && t->unit && t->unit->hitpoints > 0 && t->unit->kind != this->kind) {
            if (!target || t->unit->hitpoints < target->hitpoints ||
                (t->unit->hitpoints == target->hitpoints &&
                 (t->y < target->tile->y || (t->y == target->tile->y && t->x < target->tile->x)))) {
                target = t->unit;
            }
        }
    }
    return target;
}

void Unit::move(Cave* c) {
    if (this->enemy_neighbor(c)) {
        return;
    }

    std::pair<std::map<Tile*, int>, std::map<Tile*, Tile*>> bfs_result = find_walkable_tiles(c, this->tile);
    const auto& distances = bfs_result.first;
    const auto& path = bfs_result.second;

    std::vector<Tile*> reachable_targets_adjacent_to_enemies;
    int closest_target_distance = std::numeric_limits<int>::max();

    for (Unit* enemy : this->enemies(c)) {
        for (Tile* target_tile_candidate : c->get_walkable_neighbors(enemy->tile)) {
            auto it = distances.find(target_tile_candidate);
            if (it != distances.end()) {
                int dist = it->second;
                if (dist < closest_target_distance) {
                    closest_target_distance = dist;
                    reachable_targets_adjacent_to_enemies.clear();
                }
                if (dist == closest_target_distance) {
                    reachable_targets_adjacent_to_enemies.push_back(target_tile_candidate);
                }
            }
        }
    }

    if (!reachable_targets_adjacent_to_enemies.empty()) {
        std::sort(reachable_targets_adjacent_to_enemies.begin(), reachable_targets_adjacent_to_enemies.end(), [](Tile* a, Tile* b) {
            if (a->y != b->y) {
                return a->y < b->y;
            }
            return a->x < b->x;
        });

        Tile* chosen_target_tile = reachable_targets_adjacent_to_enemies[0];
        Tile* next_step_tile = chosen_target_tile;

        while (path.at(next_step_tile) != this->tile) {
            next_step_tile = path.at(next_step_tile);
        }

        next_step_tile->unit = this;
        next_step_tile->kind = this->kind;
        this->tile->kind = KIND_SPACE;
        this->tile->unit = nullptr;
        this->tile = next_step_tile;
    }
}

bool Unit::attack(Cave* c) {
    Unit* enemy = this->enemy_neighbor(c);
    if (enemy) {
        bool killed = enemy->damage(c, this->power);
        return killed && enemy->kind == KIND_ELF;
    }
    return false;
}

bool Unit::damage(Cave* c, int damage) {
    this->hitpoints -= damage;
    if (this->hitpoints <= 0) {
        c->remove_unit(this);
        return true;
    }
    return false;
}

Cave::Cave(const std::vector<std::string>& input, int elf_power) {
    parse_map(input, elf_power);
}

void Cave::parse_map(const std::vector<std::string>& input, int elf_power) {
    int rows = input.size();
    int cols = input[0].size();
    map.resize(rows, std::vector<Tile>(cols));

    for (int y = 0; y < rows; ++y) {
        for (int x = 0; x < cols; ++x) {
            char c = input[y][x];
            Kind k = RUNE_KINDS.at(c);
            map[y][x] = Tile(k, x, y);

            if (is_unit_kind(k)) {
                units.push_back(std::make_shared<Unit>(&map[y][x], k, elf_power));
                map[y][x].unit = units.back().get();
            }
        }
    }
}

std::pair<int, bool> Cave::status() const {
    bool elves_exist = false;
    bool goblins_exist = false;
    int total_hp = 0;

    for (const auto& u_ptr : units) {
        if (u_ptr->hitpoints > 0) {
            if (u_ptr->kind == KIND_ELF) {
                elves_exist = true;
            } else {
                goblins_exist = true;
            }
            total_hp += u_ptr->hitpoints;
        }
    }
    return {total_hp, elves_exist && goblins_exist};
}

void Cave::remove_the_dead() {
    units.erase(std::remove_if(units.begin(), units.end(),
                                [](const std::shared_ptr<Unit>& u_ptr){
                                    return u_ptr->hitpoints <= 0;
                                }),
                units.end());
}

void Cave::remove_unit(Unit* u) {
    if (u->tile) {
        u->tile->kind = KIND_SPACE;
        u->tile->unit = nullptr;
        u->tile = nullptr;
    }
}

std::pair<bool, bool> Cave::tick(bool stop_on_elf_death) {
    remove_the_dead();

    std::sort(units.begin(), units.end(), [](const std::shared_ptr<Unit>& a, const std::shared_ptr<Unit>& b) {
        if (a->tile->y != b->tile->y) {
            return a->tile->y < b->tile->y;
        }
        return a->tile->x < b->tile->x;
    });

    bool clean_round = true;
    bool elf_died_in_round = false;

    for (const auto& u_ptr : units) {
        if (u_ptr->hitpoints <= 0) {
            continue;
        }

        if (!u_ptr->targets(this)) {
            clean_round = false;
            break;
        }

        u_ptr->move(this);
        if (u_ptr->attack(this) && stop_on_elf_death) {
            elf_died_in_round = true;
            clean_round = false;
            break;
        }
    }
    return {clean_round, elf_died_in_round};
}

std::pair<std::map<Tile*, int>, std::map<Tile*, Tile*>> find_walkable_tiles(const Cave* c, Tile* t) {
    std::queue<Tile*> frontier;
    frontier.push(t);

    std::map<Tile*, int> distance;
    distance[t] = 0;

    std::map<Tile*, Tile*> came_from;
    came_from[t] = nullptr;

    while (!frontier.empty()) {
        Tile* current = frontier.front();
        frontier.pop();

        for (Tile* next : c->get_walkable_neighbors(current)) {
            if (distance.find(next) == distance.end()) {
                frontier.push(next);
                distance[next] = distance[current] + 1;
                came_from[next] = current;
            }
        }
    }
    return {distance, came_from};
}

int combat(const std::vector<std::string>& input) {
    Cave cave(input, DEFAULT_POWER);
    int rounds_completed = 0;

    while (true) {
        std::pair<int, bool> current_status = cave.status();
        if (!current_status.second) {
            return rounds_completed * current_status.first;
        }

        std::pair<bool, bool> tick_result = cave.tick(false);
        if (!tick_result.first) {
            return rounds_completed * cave.status().first;
        }
        rounds_completed++;
    }
}

int cheating_elves(const std::vector<std::string>& input) {
    int power = DEFAULT_POWER + 1;
    while (true) {
        Cave cave(input, power);
        int rounds_completed = 0;
        bool elf_died_in_this_power_level = false;

        while (true) {
            std::pair<int, bool> current_status = cave.status();
            if (!current_status.second) {
                return rounds_completed * current_status.first;
            }

            std::pair<bool, bool> tick_result = cave.tick(true);
            if (tick_result.second) {
                elf_died_in_this_power_level = true;
                break;
            }

            if (!tick_result.first) {
                return rounds_completed * cave.status().first;
            }

            rounds_completed++;
        }
        power++;
    }
}

int main() {
    std::ifstream file("input.txt");
    std::vector<std::string> lines;
    std::string line;
    while (std::getline(file, line)) {
        lines.push_back(line);
    }
    file.close();

    std::cout << cheating_elves(lines) << std::endl;

    return 0;
}
