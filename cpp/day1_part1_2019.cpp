#include <iostream>
#include <fstream>
#include <vector>

int calculateFuel(int mass) {
    return (mass / 3) - 2;
}

int main() {
    std::ifstream inputFile("input.txt");
    if (!inputFile) {
        std::cerr << "Error opening input file." << std::endl;
        return 1;
    }

    int totalFuel = 0;
    int mass;
    while (inputFile >> mass) {
        totalFuel += calculateFuel(mass);
    }

    std::cout << "Total fuel requirement: " << totalFuel << std::endl;

    return 0;
}