#include <iostream>
#include <fstream>
#include <unordered_map>
#include <string>
#include <vector>

int main() {
    std::ifstream input("input.txt");
    std::string line;
    std::getline(input, line);
    input.close();

    std::vector<int> startingNumbers;
    size_t pos = 0;
    while ((pos = line.find(',')) != std::string::npos) {
        startingNumbers.push_back(std::stoi(line.substr(0, pos)));
        line.erase(0, pos + 1);
    }
    startingNumbers.push_back(std::stoi(line));

    std::unordered_map<int, int> spoken;

    int lastSpoken = 0;
    for (size_t i = 0; i < startingNumbers.size(); ++i) {
        if (i == startingNumbers.size() - 1) {
            lastSpoken = startingNumbers[i];
        } else {
            spoken[startingNumbers[i]] = i + 1;
        }
    }

    for (int turn = startingNumbers.size() + 1; turn <= 30000000; ++turn) {
        int nextNumber = 0;
        if (spoken.find(lastSpoken) != spoken.end()) {
            nextNumber = turn - 1 - spoken[lastSpoken];
        }
        spoken[lastSpoken] = turn - 1;
        lastSpoken = nextNumber;
    }

    std::cout << lastSpoken << std::endl;

    return 0;
}