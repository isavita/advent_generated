
#include <iostream>
#include <vector>
#include <string>
#include <map>
#include <queue>
#include <algorithm>
#include <memory>
#include <limits>
#include <utility>
#include <fstream>

class Tile;
class Unit;
class Cave;

enum class TileKind {
    Space,
    Elf,
    Goblin,
    Wall
};

const int DEFAULT_HITPOINTS = 200;
const int DEFAULT_POWER = 3;

struct Offset {
    int dx, dy;
};
const std::vector<Offset> OFFSETS = {
    {0, -1},
    {-1, 0},
    {1, 0},
    {0, 1},
};

std::map<char, TileKind> RUNE_KINDS = {
    {'.', TileKind::Space},
    {'E', TileKind::Elf},
    {'G', TileKind::Goblin},
    {'#', TileKind::Wall},
};

bool is_unit_kind(TileKind kind) {
    return kind == TileKind::Elf || kind == TileKind::Goblin;
}

std::pair<std::map<Tile*, int>, std::map<Tile*, Tile*>> find_walkable_tiles(
    const std::vector<std::vector<std::unique_ptr<Tile>>>& game_map, Tile* start);

class Tile {
public:
    TileKind kind;
    int x, y;
    Unit* unit;

    Tile(TileKind k, int x_coord, int y_coord) : kind(k), x(x_coord), y(y_coord), unit(nullptr) {}

    std::vector<Tile*> walkable_neighbors(const std::vector<std::vector<std::unique_ptr<Tile>>>& game_map) const {
        std::vector<Tile*> neighbors;
        int max_y = game_map.size();
        if (max_y == 0) return neighbors;
        int max_x = game_map[0].size();

        for (const auto& offset : OFFSETS) {
            int nx = x + offset.dx;
            int ny = y + offset.dy;

            if (ny >= 0 && ny < max_y && nx >= 0 && nx < max_x) {
                Tile* n_tile = game_map[ny][nx].get();
                if (n_tile && n_tile->kind == TileKind::Space) {
                    neighbors.push_back(n_tile);
                }
            }
        }
        return neighbors;
    }
};

class Unit {
public:
    TileKind kind;
    int hitpoints;
    int power;
    Tile* tile;

    Unit(Tile* t, TileKind k, int elf_power) : kind(k), hitpoints(DEFAULT_HITPOINTS), power(k == TileKind::Elf ? elf_power : DEFAULT_POWER), tile(t) {
        if (tile) {
            tile->unit = this;
        }
    }

    bool targets(const Cave& c) const;
    std::pair<Tile*, Tile*> next_tile(const Cave& c) const;
    std::vector<Unit*> enemies(const Cave& c) const;
    Unit* enemy_neighbor(const Cave& c) const;
    void move(Cave& c);
    bool attack(Cave& c);
    bool damage(Cave& c, int damage_taken);
};

class Cave {
public:
    std::vector<std::vector<std::unique_ptr<Tile>>> map;
    std::vector<std::unique_ptr<Unit>> units;

    Cave(const std::vector<std::string>& input, int elf_power) {
        parse_map(input, elf_power);
    }

    void parse_map(const std::vector<std::string>& input, int elf_power) {
        map.clear();
        units.clear();

        map.resize(input.size());
        for (int y = 0; y < input.size(); ++y) {
            map[y].resize(input[y].length());
            for (int x = 0; x < input[y].length(); ++x) {
                char col_char = input[y][x];
                TileKind kind = RUNE_KINDS.at(col_char);

                map[y][x] = std::make_unique<Tile>(kind, x, y);
                
                if (is_unit_kind(kind)) {
                    units.push_back(std::make_unique<Unit>(map[y][x].get(), kind, elf_power));
                }
            }
        }
    }

    std::pair<int, bool> status() const {
        bool elves_exist = false;
        bool goblins_exist = false;
        int total_hp = 0;

        for (const auto& u_ptr : units) {
            if (u_ptr->hitpoints <= 0) {
                continue;
            }
            if (u_ptr->kind == TileKind::Elf) {
                elves_exist = true;
            } else {
                goblins_exist = true;
            }
            total_hp += u_ptr->hitpoints;
        }
        return {total_hp, elves_exist && goblins_exist};
    }

    void remove_the_dead() {
        units.erase(std::remove_if(units.begin(), units.end(),
                                   [](const std::unique_ptr<Unit>& u_ptr){
                                       return u_ptr->hitpoints <= 0;
                                   }),
                units.end());
    }

    void remove_unit(Unit* u) {
        if (u->tile) {
            u->tile->kind = TileKind::Space;
            u->tile->unit = nullptr;
        }
        u->tile = nullptr;
    }

    std::pair<bool, bool> tick(bool stop_on_elf_death) {
        remove_the_dead();

        std::sort(units.begin(), units.end(),
                  [](const std::unique_ptr<Unit>& a, const std::unique_ptr<Unit>& b){
                      return a->tile->y < b->tile->y || (a->tile->y == b->tile->y && a->tile->x < b->tile->x);
                  });

        bool elf_died_this_round = false;
        bool all_units_acted = true;

        for (const auto& u_ptr : units) {
            if (u_ptr->hitpoints <= 0) {
                continue;
            }
            
            if (!u_ptr->targets(*this)) {
                all_units_acted = false;
                break;
            }

            u_ptr->move(*this);
            if (u_ptr->attack(*this) && stop_on_elf_death) {
                elf_died_this_round = true;
                all_units_acted = false;
                break;
            }
        }
        return {all_units_acted, elf_died_this_round};
    }
};

std::pair<std::map<Tile*, int>, std::map<Tile*, Tile*>> find_walkable_tiles(
    const std::vector<std::vector<std::unique_ptr<Tile>>>& game_map, Tile* start) {

    std::map<Tile*, int> distance;
    std::map<Tile*, Tile*> came_from;
    std::queue<Tile*> frontier;

    frontier.push(start);
    distance[start] = 0;
    came_from[start] = nullptr;

    while (!frontier.empty()) {
        Tile* current = frontier.front();
        frontier.pop();

        for (Tile* next : current->walkable_neighbors(game_map)) {
            if (distance.find(next) == distance.end()) {
                frontier.push(next);
                distance[next] = distance[current] + 1;
                came_from[next] = current;
            }
        }
    }
    return {distance, came_from};
}

bool Unit::targets(const Cave& c) const {
    for (const auto& u_ptr : c.units) {
        if (u_ptr->hitpoints > 0 && u_ptr->kind != this->kind) {
            return true;
        }
    }
    return false;
}

std::pair<Tile*, Tile*> Unit::next_tile(const Cave& c) const {
    auto bfs_result = find_walkable_tiles(c.map, this->tile);
    const auto& distances = bfs_result.first;
    const auto& path = bfs_result.second;

    std::vector<Tile*> potential_targets_in_range;
    int closest_target_distance = std::numeric_limits<int>::max();

    for (Unit* enemy : enemies(c)) {
        for (const auto& offset : OFFSETS) {
            int ny = enemy->tile->y + offset.dy;
            int nx = enemy->tile->x + offset.dx;

            if (ny >= 0 && ny < c.map.size() && nx >= 0 && nx < c.map[0].size()) {
                Tile* target_tile_candidate = c.map[ny][nx].get();
                
                if (target_tile_candidate && target_tile_candidate->kind == TileKind::Space &&
                    distances.count(target_tile_candidate) > 0) {
                    
                    int dist_to_target = distances.at(target_tile_candidate);

                    if (dist_to_target < closest_target_distance) {
                        closest_target_distance = dist_to_target;
                        potential_targets_in_range.clear();
                        potential_targets_in_range.push_back(target_tile_candidate);
                    } else if (dist_to_target == closest_target_distance) {
                        potential_targets_in_range.push_back(target_tile_candidate);
                    }
                }
            }
        }
    }

    std::sort(potential_targets_in_range.begin(), potential_targets_in_range.end(),
              [](Tile* a, Tile* b){
                  return a->y < b->y || (a->y == b->y && a->x < b->x);
              });

    if (potential_targets_in_range.empty()) {
        return {nullptr, nullptr};
    }

    Tile* chosen_target_tile = potential_targets_in_range[0];

    Tile* next_step_tile = chosen_target_tile;
    while (path.at(next_step_tile) != this->tile) {
        next_step_tile = path.at(next_step_tile);
    }
    
    return {next_step_tile, chosen_target_tile};
}

std::vector<Unit*> Unit::enemies(const Cave& c) const {
    std::vector<Unit*> enemy_units;
    for (const auto& u_ptr : c.units) {
        if (u_ptr->hitpoints > 0 && u_ptr->kind != this->kind) {
            enemy_units.push_back(u_ptr.get());
        }
    }
    std::sort(enemy_units.begin(), enemy_units.end(),
              [](const Unit* a, const Unit* b){
                  return a->tile->y < b->tile->y || (a->tile->y == b->tile->y && a->tile->x < b->tile->x);
              });
    return enemy_units;
}

Unit* Unit::enemy_neighbor(const Cave& c) const {
    Unit* target = nullptr;
    for (const auto& offset : OFFSETS) {
        int ny = tile->y + offset.dy;
        int nx = tile->x + offset.dx;

        if (ny >= 0 && ny < c.map.size() && nx >= 0 && nx < c.map[0].size()) {
            Tile* neighbor_tile = c.map[ny][nx].get();
            if (neighbor_tile && neighbor_tile->unit && 
                neighbor_tile->unit->kind != this->kind && neighbor_tile->unit->hitpoints > 0) {
                
                if (!target || neighbor_tile->unit->hitpoints < target->hitpoints ||
                    (neighbor_tile->unit->hitpoints == target->hitpoints && 
                     (neighbor_tile->unit->tile->y < target->tile->y || 
                      (neighbor_tile->unit->tile->y == target->tile->y && neighbor_tile->unit->tile->x < target->tile->x)))) {
                    target = neighbor_tile->unit;
                }
            }
        }
    }
    return target;
}

void Unit::move(Cave& c) {
    if (enemy_neighbor(c)) {
        return;
    }

    std::pair<Tile*, Tile*> move_info = next_tile(c);
    Tile* next_step = move_info.first;

    if (next_step) {
        tile->kind = TileKind::Space;
        tile->unit = nullptr;         

        next_step->kind = this->kind;
        next_step->unit = this;       

        this->tile = next_step;       
    }
}

bool Unit::attack(Cave& c) {
    Unit* enemy = enemy_neighbor(c);
    if (enemy) {
        bool killed = enemy->damage(c, this->power);
        return killed && enemy->kind == TileKind::Elf;
    }
    return false;
}

bool Unit::damage(Cave& c, int damage_taken) {
    hitpoints -= damage_taken;
    if (hitpoints <= 0) {
        c.remove_unit(this);
        return true;
    }
    return false;
}

int combat(const std::vector<std::string>& input) {
    Cave cave(input, DEFAULT_POWER);
    int rounds = 0;
    while (true) {
        rounds++;
        std::pair<int, bool> status = cave.status();
        if (!status.second) {
            return (rounds - 1) * status.first;
        }
        std::pair<bool, bool> tick_result = cave.tick(false);
        if (!tick_result.first) {
            rounds--;
        }
    }
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(nullptr);

    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cerr << "Error opening input.txt" << std::endl;
        return 1;
    }

    std::vector<std::string> lines;
    std::string line;
    while (std::getline(file, line)) {
        lines.push_back(line);
    }
    file.close();

    std::cout << combat(lines) << std::endl;

    return 0;
}

