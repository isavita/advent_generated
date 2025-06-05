
#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <algorithm>
#include <sstream>

struct Range {
    long long destStart;
    long long srcStart;
    long long length;
};

long long reverseConvertNumber(long long number, const std::vector<Range>& ranges) {
    for (const auto& r : ranges) {
        if (r.destStart <= number && number < r.destStart + r.length) {
            return r.srcStart + (number - r.destStart);
        }
    }
    return number;
}

bool isInSeedRanges(long long number, const std::vector<std::pair<long long, long long>>& seedRanges) {
    for (const auto& r : seedRanges) {
        if (r.first <= number && number < r.first + r.second) {
            return true;
        }
    }
    return false;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::ifstream file("input.txt");

    std::vector<std::pair<long long, long long>> seedRanges;
    std::vector<Range> currentRanges;
    std::vector<std::vector<Range>> maps;

    std::string line;
    while (std::getline(file, line)) {
        if (line.empty()) {
            continue;
        }

        if (line.rfind("seeds:", 0) == 0) {
            std::stringstream ss(line.substr(7));
            long long start, length;
            while (ss >> start >> length) {
                seedRanges.push_back({start, length});
            }
        } else if (line.find("map:") != std::string::npos) {
            if (!currentRanges.empty()) {
                maps.push_back(currentRanges);
                currentRanges.clear();
            }
        } else {
            std::stringstream ss(line);
            long long destStart, srcStart, length;
            if (ss >> destStart >> srcStart >> length) {
                currentRanges.push_back({destStart, srcStart, length});
            }
        }
    }

    if (!currentRanges.empty()) {
        maps.push_back(currentRanges);
    }

    long long location = 0;
    while (true) {
        long long seed = location;
        for (int i = maps.size() - 1; i >= 0; --i) {
            seed = reverseConvertNumber(seed, maps[i]);
        }

        if (isInSeedRanges(seed, seedRanges)) {
            std::cout << location << std::endl;
            break;
        }
        location++;
    }

    return 0;
}
