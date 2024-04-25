#include <fstream>
#include <iostream>
#include <string>
#include <algorithm>

std::string react(std::string polymer) {
    bool reactionOccurred;
    do {
        reactionOccurred = false;
        for (int i = 0; i < polymer.size() - 1; i++) {
            if (std::tolower(polymer[i]) == std::tolower(polymer[i + 1]) && polymer[i] != polymer[i + 1]) {
                polymer.erase(i, 2);
                reactionOccurred = true;
            }
        }
    } while (reactionOccurred);
    return polymer;
}

int main() {
    std::ifstream file("input.txt");
    if (!file) {
        std::cerr << "Error reading file" << std::endl;
        return 1;
    }

    std::string polymer((std::istreambuf_iterator<char>(file)), std::istreambuf_iterator<char>());
    polymer.erase(std::remove(polymer.begin(), polymer.end(), '\n'), polymer.end());

    int minLength = polymer.size();
    for (char unit = 'a'; unit <= 'z'; unit++) {
        std::string tempPolymer = polymer;
        tempPolymer.erase(std::remove(tempPolymer.begin(), tempPolymer.end(), unit), tempPolymer.end());
        tempPolymer.erase(std::remove(tempPolymer.begin(), tempPolymer.end(), std::toupper(unit)), tempPolymer.end());
        std::string reactedPolymer = react(tempPolymer);
        if (reactedPolymer.size() < minLength) {
            minLength = reactedPolymer.size();
        }
    }

    std::cout << minLength << std::endl;
    return 0;
}