
#include <iostream>
#include <fstream>
#include <string>
#include <regex>
#include <sstream>

int main() {
    std::ifstream file("input.txt");
    std::string input((std::istreambuf_iterator<char>(file)), std::istreambuf_iterator<char>());

    std::regex re("mul\\([0-9]{1,3},[0-9]{1,3}\\)");
    std::sregex_iterator it(input.begin(), input.end(), re);
    std::sregex_iterator end;

    int totalSum = 0;

    while (it != end) {
        std::string match = it->str();
        match = match.substr(4, match.size() - 5);
        std::istringstream ss(match);
        std::string numStr;
        int x, y;
        std::getline(ss, numStr, ',');
        x = std::stoi(numStr);
        std::getline(ss, numStr);
        y = std::stoi(numStr);
        totalSum += x * y;
        ++it;
    }

    std::cout << totalSum << std::endl;
    return 0;
}
