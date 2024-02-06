#include <iostream>
#include <fstream>
#include <string>

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cout << "Error reading file" << std::endl;
        return 1;
    }

    std::string line;
    std::getline(file, line);
    long genAStart = std::stol(line);

    std::getline(file, line);
    long genBStart = std::stol(line);

    long genAFactor = 16807;
    long genBFactor = 48271;
    long modulus = 2147483647;

    long genA = genAStart;
    long genB = genBStart;
    int matches = 0;

    for (int i = 0; i < 40000000; i++) {
        genA = (genA * genAFactor) % modulus;
        genB = (genB * genBFactor) % modulus;

        if ((genA & 0xFFFF) == (genB & 0xFFFF)) {
            matches++;
        }
    }

    std::cout << matches << std::endl;

    return 0;
}