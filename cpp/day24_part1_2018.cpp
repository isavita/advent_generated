
#include <iostream>
#include <vector>
#include <string>
#include <map>
#include <algorithm>
#include <regex>
#include <fstream>
#include <memory>
#include <numeric>
#include <sstream>

struct Group {
    int units;
    int hitPoints;
    int attackDamage;
    std::string attackType;
    int initiative;
    std::vector<std::string> immunities;
    std::vector<std::string> weaknesses;
    Group* attacker = nullptr;
    Group* target = nullptr;

    Group(int u, int hp, int ad, const std::string& at, int init,
          const std::vector<std::string>& imm, const std::vector<std::string>& weak)
        : units(u), hitPoints(hp), attackDamage(ad), attackType(at), initiative(init),
          immunities(imm), weaknesses(weak) {}

    int effectivePower() const {
        return units * attackDamage;
    }

    int damageDealt(const Group& enemy) const {
        if (units <= 0) return 0;

        for (const auto& immunity : enemy.immunities) {
            if (immunity == attackType) return 0;
        }
        for (const auto& weakness : enemy.weaknesses) {
            if (weakness == attackType) return effectivePower() * 2;
        }
        return effectivePower();
    }
};

struct Army {
    std::vector<std::shared_ptr<Group>> groups;

    bool alive() const {
        for (const auto& g : groups) {
            if (g->units > 0) return true;
        }
        return false;
    }

    void boost(int amount) {
        for (const auto& g : groups) {
            g->attackDamage += amount;
        }
    }
};

struct Initiative {
    std::vector<std::shared_ptr<Group>> groups;

    void attack() {
        std::sort(groups.begin(), groups.end(), [](const auto& a, const auto& b) {
            return b->initiative < a->initiative;
        });

        for (const auto& group : groups) {
            if (group->units > 0 && group->target && group->target->units > 0) {
                group->target->units -= group->damageDealt(*(group->target)) / group->target->hitPoints;
                if (group->target->units < 0) {
                    group->target->units = 0;
                }
            }
            if (group->target) {
                group->target->attacker = nullptr;
                group->target = nullptr;
            }
        }
    }

    void clean() {
        groups.erase(std::remove_if(groups.begin(), groups.end(),
                                    [](const auto& g) { return g->units <= 0; }),
                     groups.end());
        std::sort(groups.begin(), groups.end(), [](const auto& a, const auto& b) {
            return b->initiative < a->initiative;
        });
    }
};

struct Battlefield {
    std::map<int, Army> armies;

    void findTargets() {
        for (auto& pair : armies) {
            for (const auto& group_ptr : pair.second.groups) {
                group_ptr->attacker = nullptr;
                group_ptr->target = nullptr;
            }
        }

        std::vector<std::pair<std::shared_ptr<Group>, int>> all_active_groups_for_targeting;
        for (const auto& pair : armies) {
            int army_id = pair.first;
            for (const auto& group_ptr : pair.second.groups) {
                if (group_ptr->units > 0) {
                    all_active_groups_for_targeting.push_back({group_ptr, army_id});
                }
            }
        }

        std::sort(all_active_groups_for_targeting.begin(), all_active_groups_for_targeting.end(),
                  [](const auto& a_pair, const auto& b_pair) {
                      const auto& a = a_pair.first;
                      const auto& b = b_pair.first;
                      int powerA = a->effectivePower();
                      int powerB = b->effectivePower();
                      if (powerA != powerB) {
                          return powerA > powerB;
                      }
                      return a->initiative > b->initiative;
                  });

        for (const auto& group_pair : all_active_groups_for_targeting) {
            const auto& group = group_pair.first;
            int current_group_army_id = group_pair.second;

            if (group->units <= 0) continue;

            int mostDamage = 0;
            Group* targetGroupRawPtr = nullptr;

            for (auto& enemy_army_pair : armies) {
                int enemyArmyId = enemy_army_pair.first;
                if (current_group_army_id == enemyArmyId) {
                    continue;
                }

                for (const auto& enemyGroup : enemy_army_pair.second.groups) {
                    if (enemyGroup->units <= 0 || enemyGroup->attacker != nullptr) {
                        continue;
                    }

                    int damage = group->damageDealt(*enemyGroup);
                    if (damage == 0) {
                        continue;
                    }

                    if (damage > mostDamage) {
                        mostDamage = damage;
                        targetGroupRawPtr = enemyGroup.get();
                    } else if (damage == mostDamage && targetGroupRawPtr) {
                        if (enemyGroup->effectivePower() > targetGroupRawPtr->effectivePower()) {
                            targetGroupRawPtr = enemyGroup.get();
                        } else if (enemyGroup->effectivePower() == targetGroupRawPtr->effectivePower() &&
                                   enemyGroup->initiative > targetGroupRawPtr->initiative) {
                            targetGroupRawPtr = enemyGroup.get();
                        }
                    }
                }
            }

            if (targetGroupRawPtr) {
                group->target = targetGroupRawPtr;
                targetGroupRawPtr->attacker = group.get();
            }
        }
    }

    void clean() {
        for (auto& pair : armies) {
            pair.second.groups.erase(std::remove_if(pair.second.groups.begin(), pair.second.groups.end(),
                                                    [](const auto& g) { return g->units <= 0; }),
                                     pair.second.groups.end());
        }
    }

    bool active() const {
        int active_armies_count = 0;
        for (const auto& pair : armies) {
            if (pair.second.alive()) {
                active_armies_count++;
            }
        }
        return active_armies_count == 2;
    }

    std::pair<int, int> result() const {
        int winner_id = 0;
        int units = 0;
        for (const auto& pair : armies) {
            if (pair.second.alive()) {
                winner_id = pair.first;
                units = std::accumulate(pair.second.groups.begin(), pair.second.groups.end(), 0,
                                        [](int sum, const auto& g) { return sum + g->units; });
                break;
            }
        }
        return {winner_id, units};
    }

    int totalUnits() const {
        int total = 0;
        for (const auto& pair : armies) {
            total += std::accumulate(pair.second.groups.begin(), pair.second.groups.end(), 0,
                                     [](int sum, const auto& g) { return sum + g->units; });
        }
        return total;
    }
};

std::vector<std::string> parse_comma_separated(const std::string& s) {
    std::vector<std::string> result;
    std::string current_item;
    std::istringstream iss(s);
    while (std::getline(iss, current_item, ',')) {
        current_item.erase(0, current_item.find_first_not_of(" \t"));
        current_item.erase(current_item.find_last_not_of(" \t") + 1);
        if (!current_item.empty()) {
            result.push_back(current_item);
        }
    }
    return result;
}

std::pair<Battlefield, Initiative> parseInput(const std::string& inputData) {
    Battlefield battle;
    Initiative initiative;
    int currentArmyId = 0;

    std::regex armyNamePattern("^([A-Za-z ]+):$");
    std::regex groupImmunitiesPattern("immune to ([a-z, ]+)");
    std::regex groupWeaknessesPattern("weak to ([a-z, ]+)");
    std::regex groupDescriptionPattern("^(\\d+) units each with (\\d+) hit points(?: \\([^)]*\\))? with an attack that does (\\d+) (\\w+) damage at initiative (\\d+)$");

    std::istringstream iss(inputData);
    std::string line;

    while (std::getline(iss, line)) {
        if (line.empty()) continue;

        std::smatch match;
        if (std::regex_match(line, match, armyNamePattern)) {
            std::string armyName = match[1].str();
            if (armyName == "Immune System") {
                currentArmyId = 1;
            } else if (armyName == "Infection") {
                currentArmyId = 2;
            }
            battle.armies[currentArmyId] = Army();
        } else {
            if (currentArmyId == 0) {
                continue;
            }

            if (std::regex_match(line, match, groupDescriptionPattern)) {
                int units = std::stoi(match[1].str());
                int hitPoints = std::stoi(match[2].str());
                int attackDamage = std::stoi(match[3].str());
                std::string attackType = match[4].str();
                int initiativeValue = std::stoi(match[5].str());

                std::vector<std::string> immunities;
                std::vector<std::string> weaknesses;

                std::smatch immunities_match;
                if (std::regex_search(line, immunities_match, groupImmunitiesPattern)) {
                    immunities = parse_comma_separated(immunities_match[1].str());
                }
                std::smatch weaknesses_match;
                if (std::regex_search(line, weaknesses_match, groupWeaknessesPattern)) {
                    weaknesses = parse_comma_separated(weaknesses_match[1].str());
                }

                auto group_ptr = std::make_shared<Group>(units, hitPoints, attackDamage, attackType, initiativeValue, immunities, weaknesses);
                battle.armies[currentArmyId].groups.push_back(group_ptr);
                initiative.groups.push_back(group_ptr);
            }
        }
    }
    return {battle, initiative};
}

int conditionFight(const std::string& inputData) {
    auto [battle, initiative] = parseInput(inputData);

    int previousUnits = -1;

    while (battle.active()) {
        battle.findTargets();
        
        bool targets_assigned_this_round = false;
        for (const auto& army_pair : battle.armies) {
            for (const auto& g_ptr : army_pair.second.groups) {
                if (g_ptr->units > 0 && g_ptr->target != nullptr) {
                    targets_assigned_this_round = true;
                    break;
                }
            }
            if (targets_assigned_this_round) break;
        }

        if (!targets_assigned_this_round) {
            return 0;
        }

        initiative.attack();
        battle.clean();
        initiative.clean();

        int currentUnits = battle.totalUnits();
        if (currentUnits == previousUnits) {
            return 0;
        }
        previousUnits = currentUnits;
    }

    auto [winner_id, units] = battle.result();
    return units;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::ifstream inputFile("input.txt");
    if (!inputFile.is_open()) {
        return 1;
    }

    std::string inputData((std::istreambuf_iterator<char>(inputFile)),
                          std::istreambuf_iterator<char>());
    inputFile.close();

    std::cout << conditionFight(inputData) << std::endl;

    return 0;
}

