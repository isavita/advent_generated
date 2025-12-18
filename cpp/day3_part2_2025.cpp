#include <iostream>
#include <fstream>
#include <string>
#include <algorithm>

const int TARGET = 12;

std::string add_big(const std::string &sum, const std::string &num) {
    int i = (int)sum.size() - 1;
    int j = (int)num.size() - 1;
    int carry = 0;
    std::string res;
    res.reserve(std::max(sum.size(), num.size()) + 1);
    while (i >= 0 || j >= 0 || carry) {
        int d1 = (i >= 0) ? sum[i] - '0' : 0;
        int d2 = (j >= 0) ? num[j] - '0' : 0;
        int t = d1 + d2 + carry;
        res.push_back(char('0' + (t % 10)));
        carry = t / 10;
        i--; j--;
    }
    std::reverse(res.begin(), res.end());
    return res;
}

int main() {
    std::ifstream infile("input.txt");
    if (!infile) return 1;

    std::string total = "0";
    std::string line;
    while (std::getline(infile, line)) {
        while (!line.empty() && !std::isdigit(static_cast<unsigned char>(line.back()))) line.pop_back();
        if (line.size() < TARGET) continue;

        int rem = (int)line.size() - TARGET;
        std::string cur;
        cur.reserve(line.size());
        for (char c : line) {
            while (rem > 0 && !cur.empty() && cur.back() < c) {
                cur.pop_back();
                --rem;
            }
            cur.push_back(c);
        }
        if (cur.size() > TARGET) cur.resize(TARGET);

        total = add_big(total, cur);
    }

    std::cout << total << "\n";
    return 0;
}