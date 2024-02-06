
#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>
#include <sstream>

struct ipRange {
    uint32_t start;
    uint32_t end;
};

std::vector<ipRange> readIPRanges(const std::string& filename);
uint32_t findUnblockedIP(const std::vector<ipRange>& ranges);

int main() {
    std::vector<ipRange> ipRanges = readIPRanges("input.txt");
    std::sort(ipRanges.begin(), ipRanges.end(), [](const ipRange& a, const ipRange& b) {
        return a.start < b.start;
    });

    uint32_t unblockedIP = findUnblockedIP(ipRanges);
    std::cout << unblockedIP << std::endl;

    return 0;
}

std::vector<ipRange> readIPRanges(const std::string& filename) {
    std::ifstream file(filename);
    if (!file.is_open()) {
        throw std::runtime_error("Error opening file");
    }

    std::vector<ipRange> ranges;
    std::string line;
    while (std::getline(file, line)) {
        std::istringstream iss(line);
        std::string startStr, endStr;
        std::getline(iss, startStr, '-');
        std::getline(iss, endStr);

        uint32_t start = std::stoul(startStr);
        uint32_t end = std::stoul(endStr);
        ranges.push_back({start, end});
    }

    return ranges;
}

uint32_t findUnblockedIP(const std::vector<ipRange>& ranges) {
    uint32_t currentIP = 0;
    for (const auto& r : ranges) {
        if (r.start > currentIP) {
            return currentIP;
        }
        if (r.end >= currentIP) {
            currentIP = r.end + 1;
        }
    }
    return currentIP;
}
