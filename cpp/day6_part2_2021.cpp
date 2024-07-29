#include <iostream>
#include <fstream>
#include <sstream>
#include <array>

int main() {
    std::ifstream file("input.txt");
    std::string line;
    std::array<long long, 9> lanternFishCounts = {0};

    if (file) {
        std::getline(file, line);
        std::stringstream ss(line);
        std::string age;
        while (std::getline(ss, age, ',')) {
            lanternFishCounts[std::stoi(age)]++;
        }
    }

    for (int i = 0; i < 256; i++) {
        long long newLanternFish = lanternFishCounts[0];
        for (int j = 0; j < 8; j++) {
            lanternFishCounts[j] = lanternFishCounts[j + 1];
        }
        lanternFishCounts[6] += newLanternFish;
        lanternFishCounts[8] = newLanternFish;
    }

    long long total = 0;
    for (auto count : lanternFishCounts) {
        total += count;
    }

    std::cout << total << std::endl;
    return 0;
}