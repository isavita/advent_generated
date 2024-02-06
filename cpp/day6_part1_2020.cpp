#include <iostream>
#include <fstream>
#include <unordered_map>

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cout << "Error opening file" << std::endl;
        return 1;
    }

    int totalCount = 0;
    std::unordered_map<char, bool> groupAnswers;

    std::string line;
    while (std::getline(file, line)) {
        if (line.empty()) {
            totalCount += groupAnswers.size();
            groupAnswers.clear();
        } else {
            for (char question : line) {
                groupAnswers[question] = true;
            }
        }
    }

    totalCount += groupAnswers.size();
    std::cout << totalCount << std::endl;

    return 0;
}