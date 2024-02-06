#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>
#include <sstream>
#include <unordered_set>

std::string sortString(std::string w) {
    std::sort(w.begin(), w.end());
    return w;
}

int main() {
    std::ifstream inputFile("input.txt");
    if (!inputFile.is_open()) {
        std::cout << "File reading error" << std::endl;
        return 1;
    }

    std::vector<std::string> passphrases;
    std::string line;
    while (std::getline(inputFile, line)) {
        passphrases.push_back(line);
    }

    int validCount = 0;

    for (const std::string& passphrase : passphrases) {
        std::istringstream iss(passphrase);
        std::unordered_set<std::string> wordSet;

        bool valid = true;
        std::string word;
        while (iss >> word) {
            std::string sortedWord = sortString(word);
            if (wordSet.find(sortedWord) != wordSet.end()) {
                valid = false;
                break;
            }
            wordSet.insert(sortedWord);
        }

        if (valid) {
            validCount++;
        }
    }

    std::cout << validCount << std::endl;

    return 0;
}