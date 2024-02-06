#include <iostream>
#include <fstream>
#include <vector>
#include <unordered_map>
#include <numeric>
#include <sstream>
#include <iomanip>

void reverseSection(std::vector<int>& arr, int start, int length) {
    int n = arr.size();
    for (int i = start, j = start + length - 1; i < j; i++, j--) {
        std::swap(arr[i % n], arr[j % n]);
    }
}

std::string knotHash(const std::string& input) {
    std::vector<int> lengths;
    for (char c : input) {
        lengths.push_back(static_cast<int>(c));
    }
    lengths.insert(lengths.end(), { 17, 31, 73, 47, 23 });

    std::vector<int> list(256);
    std::iota(list.begin(), list.end(), 0);

    int position = 0;
    int skip = 0;
    for (int round = 0; round < 64; round++) {
        for (int length : lengths) {
            reverseSection(list, position, length);
            position += length + skip;
            skip++;
        }
    }

    std::vector<int> denseHash(16);
    for (int i = 0; i < 16; i++) {
        int xorResult = 0;
        for (int j = 0; j < 16; j++) {
            xorResult ^= list[i * 16 + j];
        }
        denseHash[i] = xorResult;
    }

    std::ostringstream oss;
    for (int v : denseHash) {
        oss << std::hex << std::setw(2) << std::setfill('0') << v;
    }
    return oss.str();
}

std::string hexToBinary(const std::string& hexStr) {
    std::string binaryStr;
    for (char hexDigit : hexStr) {
        unsigned long val = std::stoul(std::string(1, hexDigit), 0, 16);
        binaryStr += std::bitset<4>(val).to_string();
    }
    return binaryStr;
}

int main() {
    std::ifstream input("input.txt");
    if (!input.is_open()) {
        std::cout << "File reading error" << std::endl;
        return 1;
    }

    std::string keyString;
    input >> keyString;
    input.close();

    int totalUsed = 0;
    for (int i = 0; i < 128; i++) {
        std::string rowKey = keyString + "-" + std::to_string(i);
        std::string hash = knotHash(rowKey);
        std::string binaryRow = hexToBinary(hash);

        for (char bit : binaryRow) {
            if (bit == '1') {
                totalUsed++;
            }
        }
    }

    std::cout << totalUsed << std::endl;
    return 0;
}