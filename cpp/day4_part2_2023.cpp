
#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <unordered_set>
#include <unordered_map>
#include <cmath>

std::vector<std::string> split(const std::string &s, char delimiter) {
    std::vector<std::string> tokens;
    std::string token;
    std::istringstream tokenStream(s);
    while (std::getline(tokenStream, token, delimiter)) {
        tokens.push_back(token);
    }
    return tokens;
}

int main() {
    std::ifstream inputFile("input.txt");
    if (!inputFile.is_open()) {
        std::cerr << "Could not open the file - 'input.txt'" << std::endl;
        return 1;
    }

    std::string line;
    int totalPoints = 0;
    std::unordered_map<int, int> cardCounts;
    int cardIndex = 1;

    while (std::getline(inputFile, line)) {
        std::vector<std::string> parts = split(line, ':');
        std::vector<std::string> numbers = split(parts[1], '|');

        std::unordered_set<int> winningNumbers;
        std::vector<int> myNumbers;

        for (const std::string &num : split(numbers[0], ' ')) {
            if (!num.empty()) {
                winningNumbers.insert(std::stoi(num));
            }
        }

        for (const std::string &num : split(numbers[1], ' ')) {
            if (!num.empty()) {
                myNumbers.push_back(std::stoi(num));
            }
        }

        int matches = 0;
        for (int num : myNumbers) {
            if (winningNumbers.find(num) != winningNumbers.end()) {
                matches++;
            }
        }

        if (matches > 0) {
            totalPoints += std::pow(2, matches - 1);
        }

        cardCounts[cardIndex] += 1;
        for (int i = 1; i <= matches; ++i) {
            cardCounts[cardIndex + i] += cardCounts[cardIndex];
        }

        cardIndex++;
    }

    inputFile.close();

    int totalCards = 0;
    for (const auto &pair : cardCounts) {
        totalCards += pair.second;
    }

    std::cout << "Total points: " << totalPoints << std::endl;
    std::cout << "Total scratchcards: " << totalCards << std::endl;

    return 0;
}
