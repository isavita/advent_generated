
#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cout << "Error opening file" << std::endl;
        return 1;
    }

    std::vector<int> fishes(9, 0);
    std::string line;
    std::getline(file, line);
    std::stringstream ss(line);
    std::string fishStr;
    while (std::getline(ss, fishStr, ',')) {
        int fish = std::stoi(fishStr);
        fishes[fish]++;
    }

    for (int day = 1; day <= 80; day++) {
        int newFish = fishes[0];
        for (int i = 1; i < fishes.size(); i++) {
            fishes[i - 1] = fishes[i];
        }
        fishes[6] += newFish;
        fishes[8] = newFish;
    }

    int totalFish = 0;
    for (int fish : fishes) {
        totalFish += fish;
    }

    std::cout << totalFish << std::endl;

    return 0;
}
