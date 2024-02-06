#include <iostream>
#include <fstream>
#include <vector>
#include <cmath>

int processLine(std::string line) {
    return std::stoi(line);
}

int calcFuelMass(int mass) {
    int fuel = (std::floor(static_cast<double>(mass) / 3) - 2);
    if (fuel <= 0) {
        return 0;
    }

    return fuel + calcFuelMass(fuel);
}

int getTotal(std::vector<int> masses) {
    int total = 0;
    for (int i = 0; i < masses.size(); i++) {
        total += calcFuelMass(masses[i]);
    }
    return total;
}

int main() {
    std::ifstream file("input.txt");
    std::vector<int> masses;
    std::string line;

    while (std::getline(file, line)) {
        int n = processLine(line);
        masses.push_back(n);
    }

    int total = getTotal(masses);
    std::cout << total << std::endl;

    return 0;
}