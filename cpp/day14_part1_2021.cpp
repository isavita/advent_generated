#include <iostream>
#include <fstream>
#include <map>
#include <string>
#include <vector>

std::string applyInsertion(std::string polymer, std::map<std::string, std::string> rules) {
    std::string newPolymer = "";
    for (int i = 0; i < polymer.length() - 1; i++) {
        newPolymer += polymer[i];
        std::string sub = polymer.substr(i, 2);
        if (rules.find(sub) != rules.end()) {
            newPolymer += rules[sub];
        }
    }
    newPolymer += polymer[polymer.length() - 1];
    return newPolymer;
}

std::map<char, int> countElements(std::string polymer) {
    std::map<char, int> counts;
    for (char c : polymer) {
        counts[c]++;
    }
    return counts;
}

std::pair<int, int> minMax(std::map<char, int> counts) {
    int min = INT_MAX;
    int max = INT_MIN;
    for (auto const &count : counts) {
        if (count.second < min) {
            min = count.second;
        }
        if (count.second > max) {
            max = count.second;
        }
    }
    return std::make_pair(min, max);
}

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cerr << "Error opening file" << std::endl;
        return 1;
    }

    std::string polymer;
    std::map<std::string, std::string> rules;

    if (std::getline(file, polymer)) {
        std::string line;
        while (std::getline(file, line)) {
            if (line.empty()) {
                continue;
            }
            size_t pos = line.find(" -> ");
            rules[line.substr(0, pos)] = line.substr(pos + 4);
        }
    }

    for (int step = 0; step < 10; step++) {
        polymer = applyInsertion(polymer, rules);
    }

    std::map<char, int> counts = countElements(polymer);
    std::pair<int, int> result = minMax(counts);

    std::cout << result.second - result.first << std::endl;

    return 0;
}