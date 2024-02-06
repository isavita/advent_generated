#include <iostream>
#include <fstream>
#include <string>

bool hasDoubleAndIncreasingDigits(std::string s) {
    bool hasDouble = false;
    for (int i = 0; i < s.length() - 1; i++) {
        if (s[i] == s[i + 1]) {
            hasDouble = true;
        }
        if (s[i] > s[i + 1]) {
            return false;
        }
    }
    return hasDouble;
}

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cerr << "Error opening file" << std::endl;
        return 1;
    }

    std::string rangeStr;
    std::getline(file, rangeStr);
    file.close();

    int start = std::stoi(rangeStr.substr(0, rangeStr.find('-')));
    int end = std::stoi(rangeStr.substr(rangeStr.find('-') + 1));

    int count = 0;
    for (int i = start; i <= end; i++) {
        std::string s = std::to_string(i);
        if (hasDoubleAndIncreasingDigits(s)) {
            count++;
        }
    }

    std::cout << count << std::endl;

    return 0;
}