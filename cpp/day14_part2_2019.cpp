
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <unordered_map>
#include <algorithm>

struct Chemical {
    std::string name;
    long long amount;
};

Chemical parse_chemical(const std::string& s) {
    size_t space_pos = s.find(' ');
    long long amount = std::stoll(s.substr(0, space_pos));
    std::string name = s.substr(space_pos + 1);
    return {name, amount};
}

long long calculate_ore(
    const std::string& chem_name,
    long long amount,
    const std::unordered_map<std::string, Chemical>& reactions,
    const std::unordered_map<std::string, std::vector<Chemical>>& ingredients,
    std::unordered_map<std::string, long long>& surplus) {

    if (chem_name == "ORE") {
        return amount;
    }

    if (surplus.count(chem_name) && surplus[chem_name] >= amount) {
        surplus[chem_name] -= amount;
        return 0;
    }

    if (surplus.count(chem_name)) {
        amount -= surplus[chem_name];
        surplus[chem_name] = 0;
    }

    const Chemical& reaction_output = reactions.at(chem_name);
    long long times = (amount + reaction_output.amount - 1) / reaction_output.amount;

    long long ore_needed = 0;
    const std::vector<Chemical>& inputs = ingredients.at(chem_name);
    for (const auto& input_chem : inputs) {
        ore_needed += calculate_ore(input_chem.name, input_chem.amount * times, reactions, ingredients, surplus);
    }

    surplus[chem_name] += times * reaction_output.amount - amount;

    return ore_needed;
}

long long max_fuel(
    const std::unordered_map<std::string, Chemical>& reactions,
    const std::unordered_map<std::string, std::vector<Chemical>>& ingredients,
    long long ore_available) {

    long long low = 0;
    long long high = ore_available / reactions.at("FUEL").amount;
    long long ans = 0;

    while (low <= high) {
        long long mid = low + (high - low) / 2;
        if (mid == 0) { // Cannot produce 0 FUEL, but binary search might try it
            low = 1;
            continue;
        }

        std::unordered_map<std::string, long long> current_surplus;
        if (calculate_ore("FUEL", mid, reactions, ingredients, current_surplus) <= ore_available) {
            ans = mid;
            low = mid + 1;
        } else {
            high = mid - 1;
        }
    }
    return ans;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::ifstream file("input.txt");
    
    std::unordered_map<std::string, Chemical> reactions;
    std::unordered_map<std::string, std::vector<Chemical>> ingredients;
    std::string line;

    while (std::getline(file, line)) {
        size_t arrow_pos = line.find(" => ");
        std::string inputs_str = line.substr(0, arrow_pos);
        std::string output_str = line.substr(arrow_pos + 4);

        Chemical output_chem = parse_chemical(output_str);
        reactions[output_chem.name] = output_chem;

        size_t start = 0;
        size_t end = inputs_str.find(", ");
        while (end != std::string::npos) {
            ingredients[output_chem.name].push_back(parse_chemical(inputs_str.substr(start, end - start)));
            start = end + 2;
            end = inputs_str.find(", ", start);
        }
        ingredients[output_chem.name].push_back(parse_chemical(inputs_str.substr(start)));
    }

    long long ore_available = 1000000000000LL;
    std::cout << max_fuel(reactions, ingredients, ore_available) << std::endl;

    return 0;
}

