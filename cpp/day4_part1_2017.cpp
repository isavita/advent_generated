#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <unordered_set>

int main() {
    std::ifstream file("input.txt");
    std::string line;
    int validCount = 0;

    while (std::getline(file, line)) {
        std::istringstream iss(line);
        std::unordered_set<std::string> wordSet;
        bool valid = true;
        std::string word;

        while (iss >> word) {
            if (wordSet.find(word) != wordSet.end()) {
                valid = false;
                break;
            }
            wordSet.insert(word);
        }

        if (valid) {
            validCount++;
        }
    }

    std::cout << validCount << std::endl;

    return 0;
}