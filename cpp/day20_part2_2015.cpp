#include <iostream>
#include <fstream>
#include <vector>

int main() {
    std::ifstream input("input.txt");
    int target;
    input >> target;

    int max_houses = 50;
    int multiplier = 11;

    std::vector<int> houses(target / 10, 0);

    for (int elf = 1; elf < target / 10; ++elf) {
        for (int house = elf, count = 0; house < target / 10 && count < max_houses; house += elf, ++count) {
            houses[house] += elf * multiplier;
        }
    }

    for (int i = 0; i < houses.size(); ++i) {
        if (houses[i] >= target) {
            std::cout << i * 10 << std::endl;
            break;
        }
    }

    return 0;
}