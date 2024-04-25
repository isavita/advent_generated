#include <fstream>
#include <vector>
#include <algorithm>
#include <iostream>
#include <sstream>
#include <stdexcept>

int calculateFuel(int currentPosition, int newPosition) {
    return abs(currentPosition - newPosition);
}

int abs(int n) {
    if (n < 0) {
        return -n;
    }
    return n;
}

int main() {
    std::ifstream file("input.txt");
    if (!file) {
        std::cerr << "Error opening file" << std::endl;
        return 1;
    }

    std::vector<int> positions;
    std::string line;
    while (std::getline(file, line)) {
        std::istringstream iss(line);
        std::string num_str;
        while (std::getline(iss, num_str, ',')) {
            int num;
            try {
                num = std::stoi(num_str);
            } catch (const std::invalid_argument& ia) {
                std::cerr << "Invalid argument: " << ia.what() << std::endl;
                return 1;
            }
            positions.push_back(num);
        }
    }

    std::sort(positions.begin(), positions.end());

    int min_fuel = INT_MAX;
    for (int i = positions[0]; i <= positions.back(); i++) {
        int fuel = 0;
        for (int pos : positions) {
            fuel += calculateFuel(pos, i);
        }
        if (fuel < min_fuel) {
            min_fuel = fuel;
        }
    }
    std::cout << min_fuel << std::endl;

    return 0;
}