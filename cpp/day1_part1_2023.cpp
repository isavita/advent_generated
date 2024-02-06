#include <iostream>
#include <fstream>
#include <string>
#include <cctype>

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cout << "Error opening file" << std::endl;
        return 1;
    }

    int sum = 0;
    std::string line;

    while (std::getline(file, line)) {
        if (line.empty()) {
            continue;
        }

        int firstDigit = -1, lastDigit = -1;

        for (char c : line) {
            if (std::isdigit(c)) {
                if (firstDigit == -1) {
                    firstDigit = c - '0';
                }
                lastDigit = c - '0';
            }
        }

        if (firstDigit != -1 && lastDigit != -1) {
            int value = std::stoi(std::to_string(firstDigit) + std::to_string(lastDigit));
            sum += value;
        }
    }

    file.close();

    std::cout << sum << std::endl;

    return 0;
}