
#include <iostream>
#include <fstream>
#include <sstream>
#include <unordered_map>
#include <vector>

int main() {
    std::ifstream inputFile("input.txt");
    std::string line;
    std::getline(inputFile, line);
    inputFile.close();

    std::vector<int> startingNumbers;
    std::stringstream ss(line);
    std::string token;
    while (std::getline(ss, token, ',')) {
        startingNumbers.push_back(std::stoi(token));
    }

    std::unordered_map<int, int> lastSpoken;
    int lastNumber = 0, nextNumber = 0;

    for (int turn = 1; turn <= 2020; turn++) {
        if (turn - 1 < startingNumbers.size()) {
            lastNumber = startingNumbers[turn-1];
            lastSpoken[lastNumber] = turn;
            continue;
        }
        if (lastSpoken.find(lastNumber) != lastSpoken.end() && lastSpoken[lastNumber] != turn - 1) {
            nextNumber = turn - 1 - lastSpoken[lastNumber];
        } else {
            nextNumber = 0;
        }
        lastSpoken[lastNumber] = turn - 1;
        lastNumber = nextNumber;
    }

    std::cout << lastNumber << std::endl;

    return 0;
}
