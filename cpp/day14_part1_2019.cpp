
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <map>
#include <sstream>
#include <algorithm> // For std::remove_if, though not strictly needed with improved parsing

struct Chemical {
    std::string name;
    long long amount;
};

Chemical parseChemical(const std::string& s) {
    std::istringstream iss(s);
    Chemical c;
    iss >> c.amount >> c.name;
    return c;
}

long long calculateOre(
    const std::string& chemName,
    long long requiredAmount,
    const std::map<std::string, Chemical>& reactions,
    const std::map<std::string, std::vector<Chemical>>& ingredients,
    std::map<std::string, long long>& surplus
) {
    if (chemName == "ORE") {
        return requiredAmount;
    }

    long long existingSurplus = 0;
    auto it = surplus.find(chemName);
    if (it != surplus.end()) {
        existingSurplus = it->second;
    }

    if (existingSurplus >= requiredAmount) {
        surplus[chemName] -= requiredAmount;
        return 0;
    }

    requiredAmount -= existingSurplus;
    surplus[chemName] = 0;

    const Chemical& reactionOutput = reactions.at(chemName);
    long long reactionAmount = reactionOutput.amount;

    long long times = (requiredAmount + reactionAmount - 1) / reactionAmount;

    long long oreNeeded = 0;
    
    const auto& inputs = ingredients.at(chemName);
    for (const auto& ingredient : inputs) {
        oreNeeded += calculateOre(
            ingredient.name,
            ingredient.amount * times,
            reactions,
            ingredients,
            surplus
        );
    }

    surplus[chemName] += times * reactionAmount - requiredAmount;

    return oreNeeded;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cerr << "Error opening input.txt" << std::endl;
        return 1;
    }

    std::map<std::string, Chemical> reactions;
    std::map<std::string, std::vector<Chemical>> ingredients;
    std::map<std::string, long long> surplus;

    std::string line;
    while (std::getline(file, line)) {
        size_t arrowPos = line.find(" => ");
        std::string inputPart = line.substr(0, arrowPos);
        std::string outputPart = line.substr(arrowPos + 4);

        Chemical outputChem = parseChemical(outputPart);
        reactions[outputChem.name] = outputChem;

        std::vector<Chemical> inputChems;
        std::istringstream iss(inputPart);
        std::string chemStr;
        while (std::getline(iss, chemStr, ',')) {
            size_t first = chemStr.find_first_not_of(' ');
            if (std::string::npos == first) {
                continue; // Empty or all-space string
            }
            size_t last = chemStr.find_last_not_of(' ');
            inputChems.push_back(parseChemical(chemStr.substr(first, (last - first + 1))));
        }
        ingredients[outputChem.name] = inputChems;
    }

    file.close();

    long long result = calculateOre("FUEL", 1, reactions, ingredients, surplus);
    std::cout << result << std::endl;

    return 0;
}

