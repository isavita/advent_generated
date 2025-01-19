
#include <iostream>
#include <fstream>
#include <string>
#include <regex>

int main() {
    std::ifstream file("input.txt");
    std::string input((std::istreambuf_iterator<char>(file)), std::istreambuf_iterator<char>());

    std::regex re("(mul\\([0-9]{1,3},[0-9]{1,3}\\))|(do\\(\\))|(don't\\(\\))");
    std::smatch match;

    bool enabled = true;
    int totalSum = 0;

    auto it = input.cbegin();
    while (std::regex_search(it, input.cend(), match, re)) {
        if (match[1].matched) {
            if (enabled) {
                std::string mulStr = match[1].str();
                int commaPos = mulStr.find(',');
                int x = std::stoi(mulStr.substr(4, commaPos - 4));
                int y = std::stoi(mulStr.substr(commaPos + 1, mulStr.length() - commaPos - 2));
                totalSum += x * y;
            }
        } else if (match[2].matched) {
            enabled = true;
        } else if (match[3].matched) {
            enabled = false;
        }
        it = match.suffix().first;
    }

    std::cout << totalSum << std::endl;

    return 0;
}
