#include <iostream>
#include <fstream>
#include <vector>
#include <string>

int main() {
    std::ifstream inputFile("input.txt");
    std::vector<int> offsets;
    std::string line;

    while (std::getline(inputFile, line)) {
        offsets.push_back(std::stoi(line));
    }

    int index = 0;
    int steps = 0;

    while (index >= 0 && index < offsets.size()) {
        int jump = offsets[index];
        offsets[index]++;
        index += jump;
        steps++;
    }

    std::cout << steps << std::endl;

    return 0;
}