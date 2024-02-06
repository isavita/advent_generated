#include <iostream>
#include <fstream>
#include <vector>
#include <string>

std::vector<std::string> readInput() {
    std::vector<std::string> lines;
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        throw std::runtime_error("Error opening file");
    }

    std::string line;
    while (std::getline(file, line)) {
        lines.push_back(line);
    }

    file.close();
    return lines;
}

int parseChange(std::string change) {
    int sign = 1;
    if (change[0] == '-') {
        sign = -1;
        change = change.substr(1);
    }
    int num = std::stoi(change);
    return sign * num;
}

int main() {
    std::vector<std::string> freqChanges = readInput();
    int freq = 0;
    for (const auto& change : freqChanges) {
        freq += parseChange(change);
    }
    std::cout << freq << std::endl;
    return 0;
}