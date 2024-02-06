#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cerr << "Error opening file" << std::endl;
        return 1;
    }

    std::vector<int> caloriesList;
    int currentCalories = 0;

    std::string line;
    while (std::getline(file, line)) {
        if (line.empty()) {
            caloriesList.push_back(currentCalories);
            currentCalories = 0;
            continue;
        }

        int calories = std::stoi(line);
        currentCalories += calories;
    }

    caloriesList.push_back(currentCalories);
    std::sort(caloriesList.rbegin(), caloriesList.rend());

    int topThreeSum = 0;
    for (int i = 0; i < 3 && i < caloriesList.size(); i++) {
        topThreeSum += caloriesList[i];
    }

    std::cout << topThreeSum << std::endl;

    file.close();
    return 0;
}