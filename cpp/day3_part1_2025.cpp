#include <iostream>
#include <fstream>
#include <string>

int calc(const std::string& s) {
    int len = (int)s.size();
    if (len == 0) return 0;
    for (int d1 = 9; d1 >= 0; --d1) {
        char target = char('0' + d1);
        size_t pos = s.find(target);
        if (pos == std::string::npos || (int)pos == len - 1) continue;
        int max2 = -1;
        for (size_t i = pos + 1; i < s.size(); ++i) {
            char c = s[i];
            if (c >= '0' && c <= '9') {
                int v = c - '0';
                if (v > max2) max2 = v;
                if (max2 == 9) break;
            }
        }
        if (max2 != -1) return d1 * 10 + max2;
    }
    return 0;
}

int main() {
    std::ifstream fin("input.txt");
    if (!fin) return 1;
    std::string line;
    long long total = 0;
    while (std::getline(fin, line)) {
        if (!line.empty() && line.back() == '\r') line.pop_back();
        total += calc(line);
    }
    std::cout << total << "\n";
    return 0;
}