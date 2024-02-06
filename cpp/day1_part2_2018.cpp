#include <iostream>
#include <fstream>
#include <unordered_set>
#include <string>

int main() {
    std::ifstream inputFile("input.txt");
    if (!inputFile.is_open()) {
        std::cerr << "Error opening file" << std::endl;
        return 1;
    }

    std::unordered_set<int> frequencies;
    int currentFrequency = 0;
    frequencies.insert(currentFrequency);

    std::string line;
    while (true) {
        inputFile.clear();
        inputFile.seekg(0, std::ios::beg);
        while (std::getline(inputFile, line)) {
            int frequencyDelta = std::stoi(line);
            currentFrequency += frequencyDelta;
            if (frequencies.find(currentFrequency) != frequencies.end()) {
                std::cout << currentFrequency << std::endl;
                return 0;
            }
            frequencies.insert(currentFrequency);
        }
    }

    inputFile.close();
    return 0;
}