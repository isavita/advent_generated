
#include <iostream>
#include <fstream>
#include <string>
#include <regex>

int main() {
    std::ifstream file("input.txt");
    std::string line;
    std::getline(file, line);

    std::regex re("row (\\d+), column (\\d+)");
    std::smatch matches;
    std::regex_search(line, matches, re);

    long long row = std::stoll(matches[1]);
    long long column = std::stoll(matches[2]);

    long long pos = (row + column - 2) * (row + column - 1) / 2 + column;

    long long code = 20151125;
    long long multiplier = 252533;
    long long modulus = 33554393;

    for (long long i = 1; i < pos; ++i) {
        code = (code * multiplier) % modulus;
    }

    std::cout << code << std::endl;

    return 0;
}
