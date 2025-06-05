
#include <iostream>
#include <vector>
#include <string>
#include <map>
#include <memory>
#include <algorithm>
#include <numeric>
#include <set>
#include <regex>
#include <fstream>
#include <sstream>

class Group;
class Army;
class Battlefield;

using GroupPtr = std::shared_ptr<Group>;

class Group {
public:
    int units;
    int hit_points;
    int attack_damage;
    std::string attack_type;
    int initiative;
    std::set<std::string> immunities;
    std::set<std::string> weaknesses;

    GroupPtr attacker;
    GroupPtr target;

    Group(int u, int hp, int ad, const std::string& at, int i,
          const std::set<std::string>& imm, const std::set<std::string>& weak)
        : units(u), hit_points(hp), attack_damage(ad), attack_type(at),
          initiative(i), immunities(imm), weaknesses(weak),
          attacker(nullptr), target(nullptr) {}

    long long effective_power() const {
        return (long long)units * attack_damage;
    }

    long long damage_dealt(const GroupPtr& enemy) const {
        if (enemy->immunities.count(attack_type)) {
            return 0;
        }
        if (enemy->weaknesses.count(attack_type)) {
            return effective_power() * 2;
        }
        return effective_power();
    }
};

class Initiative {
public:
    std::vector<GroupPtr> groups;

    Initiative(std::vector<GroupPtr> all_groups) : groups(std::move(all_groups)) {
        std::sort(groups.begin(), groups.end(), [](const GroupPtr& a, const GroupPtr& b) {
            return a->initiative > b->initiative;
        });
    }

    void attack() {
        std::sort(groups.begin(), groups.end(), [](const GroupPtr& a, const GroupPtr& b) {
            return a->initiative > b->initiative;
        });

        for (const auto& group : groups) {
            if (group->units > 0 && group->target && group->target->units > 0) {
                long long damage = group->damage_dealt(group->target);
                int units_lost = damage / group->target->hit_points;
                group->target->units -= units_lost;
            }
            if (group->target) {
                group->target->attacker = nullptr;
                group->target = nullptr;
            }
        }
    }

    void clean() {
        groups.erase(std::remove_if(groups.begin(), groups.end(),
                                     [](const GroupPtr& g) { return g->units <= 0; }),
                     groups.end());
        std::sort(groups.begin(), groups.end(), [](const GroupPtr& a, const GroupPtr& b) {
            return a->initiative > b->initiative;
        });
    }
};

class Army {
public:
    std::vector<GroupPtr> groups;
    int army_id; 

    Army(int id, std::vector<GroupPtr> g = {}) : army_id(id), groups(std::move(g)) {}

    bool alive() const {
        return std::any_of(groups.begin(), groups.end(), [](const GroupPtr& g) {
            return g->units > 0;
        });
    }

    void boost(int amount) {
        for (auto& g : groups) {
            g->attack_damage += amount;
        }
    }

    Army clone() const {
        Army new_army(army_id);
        for (const auto& g : groups) {
            new_army.groups.push_back(std::make_shared<Group>(
                g->units, g->hit_points, g->attack_damage, g->attack_type,
                g->initiative, g->immunities, g->weaknesses
            ));
        }
        return new_army;
    }
};

class Battlefield {
public:
    std::map<int, Army> armies;

    Battlefield(std::map<int, Army> a) : armies(std::move(a)) {}

    Battlefield clone() const {
        std::map<int, Army> cloned_armies;
        for (const auto& pair : armies) {
            cloned_armies.emplace(pair.first, pair.second.clone());
        }
        return Battlefield(std::move(cloned_armies));
    }

    void find_targets() {
        for (auto& pair : armies) {
            for (auto& group : pair.second.groups) {
                group->target = nullptr;
                group->attacker = nullptr;
            }
        }

        for (auto& pair : armies) {
            std::sort(pair.second.groups.begin(), pair.second.groups.end(),
                      [](const GroupPtr& a, const GroupPtr& b) {
                if (a->effective_power() != b->effective_power()) {
                    return a->effective_power() > b->effective_power();
                }
                return a->initiative > b->initiative;
            });
        }

        for (auto& pair_self : armies) {
            for (const auto& group : pair_self.second.groups) {
                if (group->units <= 0) continue; 

                long long most_damage = 0;
                GroupPtr target_group = nullptr;

                for (auto& pair_enemy : armies) {
                    if (pair_self.first == pair_enemy.first) continue; 

                    for (const auto& enemy_group : pair_enemy.second.groups) {
                        if (enemy_group->units <= 0 || enemy_group->attacker) {
                            continue; 
                        }

                        long long current_damage = group->damage_dealt(enemy_group);

                        if (current_damage == 0) {
                            continue; 
                        }

                        if (target_group == nullptr || current_damage > most_damage) {
                            most_damage = current_damage;
                            target_group = enemy_group;
                        } else if (current_damage == most_damage) {
                            if (enemy_group->effective_power() > target_group->effective_power()) {
                                target_group = enemy_group;
                            } else if (enemy_group->effective_power() == target_group->effective_power()) {
                                if (enemy_group->initiative > target_group->initiative) {
                                    target_group = enemy_group;
                                }
                            }
                        }
                    }
                }
                if (target_group) {
                    group->target = target_group;
                    target_group->attacker = group;
                }
            }
        }
    }

    void clean() {
        for (auto& pair : armies) {
            pair.second.groups.erase(std::remove_if(pair.second.groups.begin(), pair.second.groups.end(),
                                                    [](const GroupPtr& g) { return g->units <= 0; }),
                                     pair.second.groups.end());
        }
    }

    bool active() const {
        return armies.at(1).alive() && armies.at(2).alive();
    }

    std::pair<int, int> result() const {
        int winner_id = 0;
        int units_left = 0;
        if (armies.at(1).alive()) {
            winner_id = 1;
            for (const auto& g : armies.at(1).groups) {
                units_left += g->units;
            }
        } else if (armies.at(2).alive()) {
            winner_id = 2;
            for (const auto& g : armies.at(2).groups) {
                units_left += g->units;
            }
        }
        return {winner_id, units_left};
    }

    long long total_units() const {
        long long total = 0;
        for (const auto& pair : armies) {
            for (const auto& g : pair.second.groups) {
                total += g->units;
            }
        }
        return total;
    }
};

struct ParsedInput {
    Battlefield initial_battlefield;
};

ParsedInput parse_input(const std::string& input_data) {
    std::map<int, Army> armies_map;
    int current_army_id = 0;

    std::regex army_name_pattern(R"(^(.*):$)");
    std::regex group_full_pattern(R"(^(\d+) units each with (\d+) hit points(?: \((.*?)\))? with an attack that does (\d+) (\w+) damage at initiative (\d+)$)");
    std::regex immunities_pattern(R"(immune to ([\w, ]+))");
    std::regex weaknesses_pattern(R"(weak to ([\w, ]+))");

    std::istringstream iss(input_data);
    std::string line;

    while (std::getline(iss, line)) {
        if (line.empty()) continue;

        std::smatch match;
        if (std::regex_match(line, match, army_name_pattern)) {
            std::string army_name = match[1].str();
            if (army_name == "Immune System") {
                current_army_id = 1;
            } else if (army_name == "Infection") {
                current_army_id = 2;
            } else {
                throw std::runtime_error("Unknown army: " + army_name);
            }
            armies_map.emplace(current_army_id, Army(current_army_id));
        } else {
            if (current_army_id == 0) {
                continue;
            }

            if (std::regex_match(line, match, group_full_pattern)) {
                int units = std::stoi(match[1].str());
                int hit_points = std::stoi(match[2].str());
                std::string modifiers_str = match[3].str(); 
                int attack_damage = std::stoi(match[4].str());
                std::string attack_type = match[5].str();
                int initiative_value = std::stoi(match[6].str());

                std::set<std::string> immunities;
                std::set<std::string> weaknesses;

                if (!modifiers_str.empty()) {
                    std::smatch mod_match;
                    
                    std::string::const_iterator search_start(modifiers_str.cbegin());
                    if (std::regex_search(search_start, modifiers_str.cend(), mod_match, immunities_pattern)) {
                        std::string imm_list = mod_match[1].str();
                        std::istringstream imm_ss(imm_list);
                        std::string imm_type;
                        while (std::getline(imm_ss, imm_type, ',')) {
                            size_t first_char = imm_type.find_first_not_of(" ");
                            if (first_char != std::string::npos) {
                                immunities.insert(imm_type.substr(first_char));
                            }
                        }
                    }
                    
                    search_start = modifiers_str.cbegin(); 
                    if (std::regex_search(search_start, modifiers_str.cend(), mod_match, weaknesses_pattern)) {
                        std::string weak_list = mod_match[1].str();
                        std::istringstream weak_ss(weak_list);
                        std::string weak_type;
                        while (std::getline(weak_ss, weak_type, ',')) {
                            size_t first_char = weak_type.find_first_not_of(" ");
                            if (first_char != std::string::npos) {
                                weaknesses.insert(weak_type.substr(first_char));
                            }
                        }
                    }
                }

                GroupPtr group = std::make_shared<Group>(units, hit_points, attack_damage,
                                                        attack_type, initiative_value,
                                                        immunities, weaknesses);
                armies_map.at(current_army_id).groups.push_back(group);
            }
        }
    }

    return {Battlefield(std::move(armies_map))};
}

std::pair<int, int> simulate_battle(const ParsedInput& initial_parsed_data, int boost_amount) {
    Battlefield battle = initial_parsed_data.initial_battlefield.clone();
    
    std::vector<GroupPtr> current_initiative_groups;
    for (const auto& pair : battle.armies) {
        for (const auto& group : pair.second.groups) {
            current_initiative_groups.push_back(group);
        }
    }
    Initiative initiative(current_initiative_groups);

    battle.armies.at(1).boost(boost_amount);

    long long prev_total_units = -1; 

    while (battle.active()) {
        long long before_units = battle.total_units();

        battle.find_targets();
        initiative.attack();

        battle.clean();
        initiative.clean();

        long long after_units = battle.total_units();

        if (after_units == before_units) {
            return {0, 0}; 
        }
    }

    return battle.result();
}

int immune_system_boost(const std::string& input_data) {
    ParsedInput initial_parsed_data = parse_input(input_data);

    int low = 0;
    int high = 200000; 
    int min_winning_boost = -1;
    int winning_units = 0;

    while (low <= high) {
        int mid = low + (high - low) / 2;
        std::pair<int, int> battle_result = simulate_battle(initial_parsed_data, mid);

        if (battle_result.first == 1) { 
            min_winning_boost = mid;
            winning_units = battle_result.second;
            high = mid - 1; 
        } else {
            low = mid + 1; 
        }
    }

    return winning_units;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cerr << "Error opening input.txt\n";
        return 1;
    }
    std::string input_data((std::istreambuf_iterator<char>(file)),
                           std::istreambuf_iterator<char>());
    file.close();

    std::cout << immune_system_boost(input_data) << std::endl;

    return 0;
}
