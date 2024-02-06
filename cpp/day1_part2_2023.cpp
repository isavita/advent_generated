
#include <iostream>
#include <fstream>
#include <string>
#include <vector>

std::pair<int, int> findFirstAndLastDigit(std::string line) {
    std::vector<std::string> digits = {"zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"};

    int firstDigit = 0, lastDigit = 0;
    for (size_t i = 0; i < line.size(); ++i) {
        std::string digitStr(1, line[i]);
        if (digitStr >= "0" && digitStr <= "9") {
            if (firstDigit == 0) {
                firstDigit = digitStr[0] - '0';
            }
            lastDigit = digitStr[0] - '0';
        } else {
            for (size_t j = 0; j < digits.size(); ++j) {
                if (line.substr(i).find(digits[j]) == 0) {
                    if (firstDigit == 0) {
                        firstDigit = j;
                    }
                    lastDigit = j;
                    break;
                }
            }
        }
    }

    return std::make_pair(firstDigit, lastDigit);
}

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cerr << "Error opening file" << std::endl;
        return 1;
    }

    int sum = 0;
    std::string line;
    while (std::getline(file, line)) {
        auto [firstDigit, lastDigit] = findFirstAndLastDigit(line);
        sum += 10 * firstDigit + lastDigit;
    }

    file.close();

    std::cout << sum << std::endl;

    return 0;
}
