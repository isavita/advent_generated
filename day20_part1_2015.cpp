#include <iostream>
#include <fstream>
#include <vector>
#include <string>

int main() {
    std::ifstream inputFile("input.txt");
    if (!inputFile.is_open()) {
        std::cerr << "Error opening file" << std::endl;
        return 1;
    }

    std::string input;
    std::getline(inputFile, input);
    inputFile.close();

    int target = std::stoi(input) / 10;

    std::vector<int> houses(target + 1, 0);

    for (int elf = 1; elf <= target; ++elf) {
        for (int house = elf; house <= target; house += elf) {
            houses[house] += elf;
        }
    }

    for (int houseNumber = 0; houseNumber < houses.size(); ++houseNumber) {
        if (houses[houseNumber] >= target) {
            std::cout << houseNumber << std::endl;
            break;
        }
    }

    return 0;
}