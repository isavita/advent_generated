
#include <iostream>
#include <fstream>
#include <string>
#include <vector>

bool isValidPassword(int password) {
    std::string s = std::to_string(password);
    bool hasDouble = false;

    for (int i = 0; i < s.length() - 1; i++) {
        if (s[i] > s[i + 1]) {
            return false;
        }
        if (s[i] == s[i + 1]) {
            if ((i == 0 || s[i] != s[i - 1]) && (i + 2 >= s.length() || s[i] != s[i + 2])) {
                hasDouble = true;
            }
        }
    }

    return hasDouble;
}

int main() {
    std::ifstream inputFile("input.txt");
    std::string rangeStr;
    std::getline(inputFile, rangeStr);

    int start = std::stoi(rangeStr.substr(0, rangeStr.find("-")));
    int end = std::stoi(rangeStr.substr(rangeStr.find("-") + 1));

    int count = 0;
    for (int i = start; i <= end; i++) {
        if (isValidPassword(i)) {
            count++;
        }
    }

    std::cout << count << std::endl;

    return 0;
}
