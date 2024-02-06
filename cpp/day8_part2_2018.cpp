
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
        std::istringstream iss(line);
        int num;
        while (iss >> num) {
            numbers.push_back(num);
        }
        file.close();
    }
    return numbers;
}

std::pair<int, int> parseTree(std::vector<int>& data, int index) {
    int childCount = data[index];
    int metaCount = data[index + 1];
    index += 2;

    std::vector<int> childValues(childCount);
    for (int i = 0; i < childCount; i++) {
        int childValue;
        std::tie(childValue, index) = parseTree(data, index);
        childValues[i] = childValue;
    }

    int value = 0;
    if (childCount == 0) {
        for (int i = 0; i < metaCount; i++) {
            value += data[index + i];
        }
    } else {
        for (int i = 0; i < metaCount; i++) {
            int metadata = data[index + i];
            if (metadata <= childCount && metadata > 0) {
                value += childValues[metadata - 1];
            }
        }
    }
    index += metaCount;

    return std::make_pair(value, index);
}

int main() {
    std::vector<int> numbers = readInput("input.txt");
    auto result = parseTree(numbers, 0);
    std::cout << result.first << std::endl;

    return 0;
}
