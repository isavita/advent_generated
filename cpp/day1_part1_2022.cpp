#include <iostream>
#include <fstream>
#include <string>

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cerr << "Error opening file" << std::endl;
        return 1;
    }

    int maxCalories = 0;
    int currentCalories = 0;
    std::string line;

    while (std::getline(file, line)) {
        if (line.empty()) {
            if (currentCalories > maxCalories) {
                maxCalories = currentCalories;
            }
            currentCalories = 0;
            continue;
        }

        int calories = std::stoi(line);
        currentCalories += calories;
    }

    if (currentCalories > maxCalories) {
        maxCalories = currentCalories;
    }

    std::cout << maxCalories << std::endl;

    file.close();
    return 0;
}