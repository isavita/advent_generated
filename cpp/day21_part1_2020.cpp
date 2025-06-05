
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <sstream>
#include <unordered_map>
#include <unordered_set>
#include <algorithm>

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        return 1;
    }

    std::unordered_map<std::string, std::unordered_set<std::string>> allergens;
    std::unordered_map<std::string, int> ingredients_count;

    std::string line;
    while (std::getline(file, line)) {
        size_t contains_pos = line.find(" (contains ");
        std::string ingredients_str = line.substr(0, contains_pos);
        std::string allergens_str = line.substr(contains_pos + 11);
        allergens_str.pop_back();

        std::unordered_set<std::string> current_ingredients_set;
        std::stringstream ss_ingredients(ingredients_str);
        std::string ingredient_token;
        while (ss_ingredients >> ingredient_token) {
            ingredients_count[ingredient_token]++;
            current_ingredients_set.insert(ingredient_token);
        }

        std::stringstream ss_allergens(allergens_str);
        std::string allergen_token;
        while (std::getline(ss_allergens, allergen_token, ',')) {
            if (!allergen_token.empty() && allergen_token[0] == ' ') {
                allergen_token = allergen_token.substr(1);
            }

            if (allergens.find(allergen_token) == allergens.end()) {
                allergens[allergen_token] = current_ingredients_set;
            } else {
                std::unordered_set<std::string> intersected_set;
                const auto& existing_set = allergens[allergen_token];

                if (existing_set.size() < current_ingredients_set.size()) {
                    for (const auto& ing : existing_set) {
                        if (current_ingredients_set.count(ing)) {
                            intersected_set.insert(ing);
                        }
                    }
                } else {
                    for (const auto& ing : current_ingredients_set) {
                        if (existing_set.count(ing)) {
                            intersected_set.insert(ing);
                        }
                    }
                }
                allergens[allergen_token] = intersected_set;
            }
        }
    }
    file.close();

    std::unordered_set<std::string> allergen_ingredients_union;
    for (const auto& pair : allergens) {
        for (const auto& ingredient : pair.second) {
            allergen_ingredients_union.insert(ingredient);
        }
    }

    long long safe_ingredients_count = 0;
    for (const auto& pair : ingredients_count) {
        if (allergen_ingredients_union.find(pair.first) == allergen_ingredients_union.end()) {
            safe_ingredients_count += pair.second;
        }
    }

    std::cout << safe_ingredients_count << std::endl;

    return 0;
}
