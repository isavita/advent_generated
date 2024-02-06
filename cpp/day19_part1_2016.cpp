#include <iostream>
#include <fstream>
#include <string>

int readInput(std::string filename) {
    std::ifstream file(filename);
    if (!file.is_open()) {
        throw std::runtime_error("Failed to open file");
    }

    int totalElves;
    file >> totalElves;
    return totalElves;
}

int findWinningElf(int totalElves) {
    int highestPowerOfTwo = 1;
    while (highestPowerOfTwo * 2 <= totalElves) {
        highestPowerOfTwo *= 2;
    }
    return (totalElves - highestPowerOfTwo) * 2 + 1;
}

int main() {
    int totalElves = readInput("input.txt");
    int winner = findWinningElf(totalElves);
    std::cout << winner << std::endl;
    return 0;
}