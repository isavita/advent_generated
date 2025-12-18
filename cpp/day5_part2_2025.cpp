#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>
#include <string>
#include <cctype>

struct Range { long long Min; long long Max; };

static inline std::string trim(const std::string& s) {
    size_t l = 0;
    while (l < s.size() && std::isspace(static_cast<unsigned char>(s[l]))) ++l;
    if (l == s.size()) return "";
    size_t r = s.size() - 1;
    while (r > l && std::isspace(static_cast<unsigned char>(s[r]))) --r;
    return s.substr(l, r - l + 1);
}

int main() {
    std::ifstream f("input.txt");
    if (!f.is_open()) return 1;

    std::vector<Range> ranges;
    ranges.reserve(8);

    std::string line;
    while (std::getline(f, line)) {
        line = trim(line);
        if (line.empty()) break;

        size_t dash = line.find('-');
        if (dash == std::string::npos) {
            return 1;
        }
        std::string a = trim(line.substr(0, dash));
        std::string b = trim(line.substr(dash + 1));
        if (a.empty() || b.empty()) return 1;

        long long minVal, maxVal;
        try { minVal = std::stoll(a); } catch (...) { return 1; }
        try { maxVal = std::stoll(b); } catch (...) { return 1; }

        if (minVal > maxVal) std::swap(minVal, maxVal);
        ranges.push_back({minVal, maxVal});
    }

    f.close();

    if (ranges.empty()) {
        std::cout << "Total fresh IDs: 0\n";
        return 0;
    }

    std::sort(ranges.begin(), ranges.end(), [](const Range& ra, const Range& rb){
        if (ra.Min < rb.Min) return true;
        if (ra.Min > rb.Min) return false;
        return ra.Max < rb.Max;
    });

    long long total = 0;
    long long currentMin = ranges[0].Min;
    long long currentMax = ranges[0].Max;

    for (size_t i = 1; i < ranges.size(); ++i) {
        const Range& next = ranges[i];
        if (next.Min <= currentMax) {
            if (next.Max > currentMax) currentMax = next.Max;
        } else {
            total += (currentMax - currentMin + 1);
            currentMin = next.Min;
            currentMax = next.Max;
        }
    }
    total += (currentMax - currentMin + 1);

    std::cout << "Total fresh IDs: " << total << "\n";
    return 0;
}