
#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <algorithm>

std::vector<std::string> filterByBit(std::vector<std::string> values, int bitIndex, char keep) {
    std::vector<std::string> filtered;
    for (const auto& val : values) {
        if (val[bitIndex] == keep) {
            filtered.push_back(val);
        }
    }
    return filtered;
}

std::string filterValues(std::vector<std::string> values, std::function<char(int, int)> criteria) {
    for (size_t i = 0; i < values[0].size(); ++i) {
        int zeros = 0, ones = 0;
        for (const auto& val : values) {
            if (val[i] == '0') {
                zeros++;
            } else {
                ones++;
            }
        }
        char keep = criteria(zeros, ones);
        values = filterByBit(values, i, keep);
        if (values.size() == 1) {
            break;
        }
    }
    return values[0];
}

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cerr << "Error opening file" << std::endl;
        return 1;
    }

    std::vector<std::string> values;
    std::string line;
    while (std::getline(file, line)) {
        values.push_back(line);
    }

    auto oxygenGeneratorRating = filterValues(values, [](int zeros, int ones) {
        if (zeros > ones) {
            return '0';
        } else {
            return '1';
        }
    });
    long long oxygenGeneratorRatingInt = std::stoll(oxygenGeneratorRating, nullptr, 2);

    auto co2ScrubberRating = filterValues(values, [](int zeros, int ones) {
        if (zeros <= ones) {
            return '0';
        } else {
            return '1';
        }
    });
    long long co2ScrubberRatingInt = std::stoll(co2ScrubberRating, nullptr, 2);

    std::cout << oxygenGeneratorRatingInt * co2ScrubberRatingInt << std::endl;

    return 0;
}
