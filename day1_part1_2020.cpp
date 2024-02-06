
#include <iostream>
#include <fstream>
#include <vector>

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        throw std::runtime_error("Unable to open file");
    }

    std::vector<int> numbers;
    std::string line;
    while (std::getline(file, line)) {
        if (line.empty()) {
            continue;
        }
        numbers.push_back(std::stoi(line));
    }

    for (size_t i = 0; i < numbers.size() - 1; i++) {
        for (size_t j = i + 1; j < numbers.size(); j++) {
            if (numbers[i] + numbers[j] == 2020) {
                std::cout << numbers[i] * numbers[j] << std::endl;
                return 0;
            }
        }
    }

    return 0;
}
