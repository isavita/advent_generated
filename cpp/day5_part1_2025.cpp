
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <algorithm>
#include <cctype>
#include <cstdlib>

struct Range { long long min, max; };

static std::string trim(const std::string &s) {
    size_t l = 0;
    while (l < s.size() && std::isspace((unsigned char)s[l])) ++l;
    if (l == s.size()) return "";
    size_t r = s.size() - 1;
    while (r > l && std::isspace((unsigned char)s[r])) --r;
    return s.substr(l, r - l + 1);
}

static bool contains(const std::vector<Range>& arr, long long x) {
    size_t l = 0, r = arr.size();
    while (l < r) {
        size_t m = (l + r) >> 1;
        if (x < arr[m].min) r = m;
        else if (x > arr[m].max) l = m + 1;
        else return true;
    }
    return false;
}

int main() {
    std::ifstream f("input.txt");
    if (!f) return 1;
    std::vector<Range> ranges;
    bool parsingRanges = true;
    long long freshCount = 0;

    std::string line;
    while (std::getline(f, line)) {
        std::string s = trim(line);
        if (s.empty()) {
            if (parsingRanges) {
                parsingRanges = false;
                if (!ranges.empty()) {
                    std::sort(ranges.begin(), ranges.end(), [](const Range &A, const Range &B){
                        if (A.min != B.min) return A.min < B.min;
                        return A.max < B.max;
                    });
                    std::vector<Range> merged;
                    merged.reserve(ranges.size());
                    for (const auto &rg : ranges) {
                        if (merged.empty() || rg.min > merged.back().max) {
                            merged.push_back(rg);
                        } else if (rg.max > merged.back().max) {
                            merged.back().max = rg.max;
                        }
                    }
                    ranges.swap(merged);
                }
                continue;
            } else {
                continue;
            }
        }
        if (parsingRanges) {
            size_t dashPos = s.find('-');
            if (dashPos == std::string::npos) {
                return 1;
            }
            std::string a = trim(s.substr(0, dashPos));
            std::string b = trim(s.substr(dashPos + 1));
            if (a.empty() || b.empty()) return 1;
            char* ep;
            errno = 0;
            long long mn = std::strtoll(a.c_str(), &ep, 10);
            if (ep == a.c_str() || errno) return 1;
            errno = 0;
            long long mx = std::strtoll(b.c_str(), &ep, 10);
            if (ep == b.c_str() || errno) return 1;
            ranges.push_back({mn, mx});
        } else {
            char* ep;
            errno = 0;
            long long id = std::strtoll(s.c_str(), &ep, 10);
            if (ep == s.c_str() || errno) return 1;
            if (!ranges.empty()) {
                if (contains(ranges, id)) freshCount++;
            }
        }
    }
    f.close();
    std::cout << "Number of fresh ingredients: " << freshCount << "\n";
    return 0;
}
