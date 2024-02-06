
#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>
#include <unordered_map>

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cout << "Error opening file" << std::endl;
        return 1;
    }

    std::vector<int> adapters;
    std::string line;
    while (std::getline(file, line)) {
        int joltage = std::stoi(line);
        adapters.push_back(joltage);
    }

    std::sort(adapters.begin(), adapters.end());
    std::unordered_map<int, int> joltDifferences = {{3, 1}};
    int previousJoltage = 0;

    for (int adapter : adapters) {
        int diff = adapter - previousJoltage;
        joltDifferences[diff]++;
        previousJoltage = adapter;
    }

    int product = joltDifferences[1] * joltDifferences[3];
    std::cout << product << std::endl;

    return 0;
}
