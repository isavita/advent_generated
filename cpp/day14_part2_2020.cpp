#include <fstream>
#include <iostream>
#include <map>
#include <regex>
#include <string>
#include <vector>

std::vector<int64_t> generateAddresses(const std::string& mask, int64_t address) {
    std::vector<int> floating;
    std::vector<int64_t> addresses;

    for (int i = 0; i < mask.size(); ++i) {
        if (mask[i] == '1') {
            address |= (1LL << (35 - i));
        } else if (mask[i] == 'X') {
            floating.push_back(35 - i);
        }
    }

    int count = 1 << floating.size();
    for (int i = 0; i < count; ++i) {
        int64_t modAddress = address;
        for (int j = 0; j < floating.size(); ++j) {
            if (!(i & (1 << j))) {
                modAddress &= ~(1LL << floating[j]);
            } else {
                modAddress |= (1LL << floating[j]);
            }
        }
        addresses.push_back(modAddress);
    }
    return addresses;
}

int main() {
    std::ifstream file("input.txt");
    if (!file) {
        std::cerr << "Error opening file.\n";
        return 1;
    }

    std::string line;
    std::string mask;
    std::map<int64_t, int64_t> mem;
    std::regex reMem("mem\\[(\\d+)\\] = (\\d+)"); // Corrected regex pattern

    while (std::getline(file, line)) {
        if (line.find("mask = ") == 0) {
            mask = line.substr(7);
        } else {
            std::smatch match;
            if (std::regex_search(line, match, reMem)) {
                int64_t address = std::stoll(match[1]);
                int64_t value = std::stoll(match[2]);
                std::vector<int64_t> addresses = generateAddresses(mask, address);
                for (int64_t addr : addresses) {
                    mem[addr] = value;
                }
            }
        }
    }

    int64_t sum = 0;
    for (auto& p : mem) {
        sum += p.second;
    }

    std::cout << sum << '\n';
    return 0;
}