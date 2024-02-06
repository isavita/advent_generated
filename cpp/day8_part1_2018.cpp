#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <sstream>

std::vector<int> readInput(std::string filename) {
    std::ifstream file(filename);
    std::vector<int> numbers;
    if (file.is_open()) {
        std::string line;
        std::getline(file, line);
        std::stringstream ss(line);
        int number;
        while (ss >> number) {
            numbers.push_back(number);
        }
        file.close();
    }
    return numbers;
}

std::pair<int, int> parseTree(std::vector<int>& data, int index) {
    int childCount = data[index];
    int metaCount = data[index + 1];
    index += 2;

    int sum = 0;
    for (int i = 0; i < childCount; i++) {
        auto result = parseTree(data, index);
        sum += result.first;
        index = result.second;
    }

    for (int i = 0; i < metaCount; i++) {
        sum += data[index + i];
    }
    index += metaCount;

    return std::make_pair(sum, index);
}

int main() {
    std::vector<int> numbers = readInput("input.txt");
    auto result = parseTree(numbers, 0);
    std::cout << result.first << std::endl;
    return 0;
}