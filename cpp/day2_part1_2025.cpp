#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <algorithm>

using u128 = __uint128_t;

static u128 str_to_u128(const std::string& s) {
    u128 v = 0;
    for (char c : s) {
        if (c >= '0' && c <= '9') v = v * 10 + (c - '0');
    }
    return v;
}

static std::string u128_to_str(u128 v) {
    if (v == 0) return "0";
    std::string s;
    while (v > 0) {
        int d = v % 10;
        s.push_back(char('0' + d));
        v /= 10;
    }
    std::reverse(s.begin(), s.end());
    return s;
}

int main() {
    std::ifstream fin("input.txt", std::ios::binary);
    if (!fin) return 1;

    fin.seekg(0, std::ios::end);
    std::size_t fsize = fin.tellg();
    fin.seekg(0, std::ios::beg);
    std::string content;
    content.resize(fsize);
    fin.read(&content[0], fsize);
    fin.close();

    std::vector<u128> ids;
    std::vector<u128> pow10(21);
    pow10[0] = 1;
    for (int i = 1; i <= 20; ++i) pow10[i] = pow10[i - 1] * 10;

    auto process_token = [&](const std::string& tok) {
        if (tok.empty()) return;
        std::size_t dash = tok.find('-');
        if (dash == std::string::npos) return;
        std::string a = tok.substr(0, dash);
        std::string b = tok.substr(dash + 1);
        if (a.empty() || b.empty()) return;
        u128 start = str_to_u128(a);
        u128 end = str_to_u128(b);
        if (start > end) { u128 t = start; start = end; end = t; }
        for (int k = 1; k <= 10; ++k) {
            u128 multiplier = pow10[k] + 1;
            u128 minSeed = pow10[k - 1];
            u128 maxSeed = pow10[k] - 1;
            u128 sMin = (start + multiplier - 1) / multiplier;
            u128 sMax = end / multiplier;
            if (sMin < minSeed) sMin = minSeed;
            if (sMax > maxSeed) sMax = maxSeed;
            if (sMin > sMax) continue;
            for (u128 seed = sMin; seed <= sMax; ++seed) {
                ids.push_back(seed * multiplier);
            }
        }
    };

    std::string token;
    for (char c : content) {
        if (std::isdigit(static_cast<unsigned char>(c)) || c == '-') {
            token.push_back(c);
        } else {
            if (!token.empty()) { process_token(token); token.clear(); }
        }
    }
    if (!token.empty()) { process_token(token); token.clear(); }

    std::sort(ids.begin(), ids.end());
    u128 sum = 0;
    bool hasPrev = false;
    u128 prev = 0;
    for (u128 v : ids) {
        if (!hasPrev || v != prev) {
            sum += v;
            prev = v;
            hasPrev = true;
        }
    }

    std::cout << u128_to_str(sum) << "\n";
    return 0;
}