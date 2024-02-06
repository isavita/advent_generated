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

    for (int i = 0; i < 5000000; i++) {
        // Generate next value for A that is a multiple of 4
        do {
            genA = (genA * genAFactor) % modulus;
        } while (genA % 4 != 0);

        // Generate next value for B that is a multiple of 8
        do {
            genB = (genB * genBFactor) % modulus;
        } while (genB % 8 != 0);

        if ((genA & 0xFFFF) == (genB & 0xFFFF)) {
            matches++;
        }
    }

    std::cout << matches << std::endl;

    return 0;
}