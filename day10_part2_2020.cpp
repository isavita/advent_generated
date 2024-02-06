#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>
#include <unordered_map>

int64_t countArrangements(const std::vector<int>& adapters) {
    std::unordered_map<int, int64_t> ways;
    ways[0] = 1;

    for (size_t i = 1; i < adapters.size(); i++) {
        int currentJoltage = adapters[i];
        for (const int diff : {1, 2, 3}) {
            ways[currentJoltage] += ways[currentJoltage - diff];
        }
    }

    return ways[adapters.back()];
}

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cout << "Error opening file" << std::endl;
        return 1;
    }

    std::vector<int> adapters{0};
    std::string line;
    while (std::getline(file, line)) {
        adapters.push_back(std::stoi(line));
    }

    std::sort(adapters.begin(), adapters.end());
    adapters.push_back(adapters.back() + 3);

    std::cout << countArrangements(adapters) << std::endl;

    return 0;
}