
#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <sstream>
#include <algorithm>
#include <limits>

struct RangeMap {
    long long srcStart;
    long long destStart;
    long long length;
};

long long convertNumber(long long number, const std::vector<RangeMap>& ranges) {
    for (const auto& r : ranges) {
        if (number >= r.srcStart && number < r.srcStart + r.length) {
            return r.destStart + (number - r.srcStart);
        }
    }
    return number;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::ifstream file("input.txt");
    std::string line;

    std::vector<long long> seeds;
    std::vector<std::vector<RangeMap>> maps;
    std::vector<RangeMap> currentRanges;

    while (std::getline(file, line)) {
        if (line.empty()) {
            continue;
        }

        if (line.find("map:") != std::string::npos) {
            if (!currentRanges.empty()) {
                maps.push_back(currentRanges);
                currentRanges.clear();
            }
        } else if (line.find("seeds:") != std::string::npos) {
            std::istringstream iss(line.substr(line.find(":") + 1));
            long long seedVal;
            while (iss >> seedVal) {
                seeds.push_back(seedVal);
            }
        } else {
            std::istringstream iss(line);
            long long destStart, srcStart, length;
            if (iss >> destStart >> srcStart >> length) {
                currentRanges.push_back({srcStart, destStart, length});
            }
        }
    }
    if (!currentRanges.empty()) {
        maps.push_back(currentRanges);
    }

    long long minLocation = std::numeric_limits<long long>::max();

    for (long long seed : seeds) {
        long long location = seed;
        for (const auto& m : maps) {
            location = convertNumber(location, m);
        }
        minLocation = std::min(minLocation, location);
    }

    std::cout << minLocation << std::endl;

    return 0;
}
