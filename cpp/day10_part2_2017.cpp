
#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>
#include <numeric>
#include <sstream>
#include <iomanip>

std::vector<int> readInputFromFile(const std::string& filename) {
    std::ifstream file(filename);
    if (!file.is_open()) {
        std::cerr << "File reading error" << std::endl;
        exit(1);
    }

    std::string input;
    std::getline(file, input);

    std::vector<int> lengths;
    for (char c : input) {
        lengths.push_back(static_cast<int>(c));
    }
    lengths.insert(lengths.end(), {17, 31, 73, 47, 23});

    return lengths;
}

int main() {
    std::vector<int> lengths = readInputFromFile("input.txt");

    std::vector<int> list(256);
    std::iota(list.begin(), list.end(), 0);

    int currentPosition = 0;
    int skipSize = 0;

    for (int round = 0; round < 64; ++round) {
        for (int length : lengths) {
            for (int i = 0; i < length / 2; ++i) {
                int start = (currentPosition + i) % 256;
                int end = (currentPosition + length - 1 - i) % 256;
                std::swap(list[start], list[end]);
            }
            currentPosition = (currentPosition + length + skipSize) % 256;
            skipSize++;
        }
    }

    std::vector<int> denseHash;
    for (int i = 0; i < 256; i += 16) {
        int xorResult = 0;
        for (int j = 0; j < 16; ++j) {
            xorResult ^= list[i + j];
        }
        denseHash.push_back(xorResult);
    }

    std::stringstream ss;
    for (int num : denseHash) {
        ss << std::hex << std::setw(2) << std::setfill('0') << num;
    }

    std::cout << ss.str() << std::endl;

    return 0;
}
