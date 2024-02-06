
#include <iostream>
#include <fstream>
#include <map>

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cerr << "Error opening file" << std::endl;
        return 1;
    }

    int totalCount = 0;
    std::map<char, int> groupAnswers;
    int groupSize = 0;

    std::string line;
    while (std::getline(file, line)) {
        if (line.empty()) {
            for (const auto& pair : groupAnswers) {
                if (pair.second == groupSize) {
                    totalCount++;
                }
            }
            groupAnswers.clear();
            groupSize = 0;
        } else {
            groupSize++;
            for (char question : line) {
                groupAnswers[question]++;
            }
        }
    }

    for (const auto& pair : groupAnswers) {
        if (pair.second == groupSize) {
            totalCount++;
        }
    }

    std::cout << totalCount << std::endl;

    return 0;
}
