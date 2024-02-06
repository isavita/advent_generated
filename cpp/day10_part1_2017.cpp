#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cout << "File reading error" << std::endl;
        return 1;
    }

    std::vector<int> lengths;
    std::string line;
    std::getline(file, line);
    std::stringstream ss(line);
    std::string lengthStr;
    while (std::getline(ss, lengthStr, ',')) {
        lengths.push_back(std::stoi(lengthStr));
    }

    std::vector<int> list(256);
    for (int i = 0; i < 256; i++) {
        list[i] = i;
    }
    int currentPosition = 0;
    int skipSize = 0;

    for (int length : lengths) {
        for (int i = 0; i < length/2; i++) {
            int start = (currentPosition + i) % 256;
            int end = (currentPosition + length - 1 - i) % 256;
            std::swap(list[start], list[end]);
        }

        currentPosition = (currentPosition + length + skipSize) % 256;
        skipSize++;
    }

    int result = list[0] * list[1];
    std::cout << result << std::endl;

    return 0;
}