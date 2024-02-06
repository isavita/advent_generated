
#include <iostream>
#include <fstream>
#include <vector>
#include <unordered_map>

const int preambleLength = 25;

bool isValid(int number, const std::vector<int>& previousNumbers) {
    std::unordered_map<int, bool> seen;
    for (int n : previousNumbers) {
        if (seen[number - n]) {
            return true;
        }
        seen[n] = true;
    }
    return false;
}

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cout << "Error opening file" << std::endl;
        return 1;
    }

    std::vector<int> numbers;
    int n;
    while (file >> n) {
        numbers.push_back(n);
    }

    for (size_t i = preambleLength; i < numbers.size(); i++) {
        if (!isValid(numbers[i], std::vector<int>(numbers.begin() + i - preambleLength, numbers.begin() + i))) {
            std::cout << numbers[i] << std::endl;
            break;
        }
    }

    return 0;
}
