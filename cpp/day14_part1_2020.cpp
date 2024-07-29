#include <iostream>
#include <fstream>
#include <string>
#include <unordered_map>
#include <bitset>

int64_t applyMask(int64_t value, const std::string& mask) {
    int64_t result = 0;
    for (int i = 0; i < 36; ++i) {
        int64_t bitValue = 1LL << (35 - i);
        if (mask[i] == '1') {
            result |= bitValue;
        } else if (mask[i] == 'X') {
            result |= (value & bitValue);
        }
    }
    return result;
}

int main() {
    std::ifstream file("input.txt");
    std::string line, mask;
    std::unordered_map<int64_t, int64_t> mem;

    while (std::getline(file, line)) {
        if (line.substr(0, 7) == "mask = ") {
            mask = line.substr(7);
        } else {
            int64_t address, value;
            sscanf(line.c_str(), "mem[%lld] = %lld", &address, &value);
            mem[address] = applyMask(value, mask);
        }
    }

    int64_t sum = 0;
    for (const auto& [_, value] : mem) {
        sum += value;
    }

    std::cout << sum << std::endl;
    return 0;
}