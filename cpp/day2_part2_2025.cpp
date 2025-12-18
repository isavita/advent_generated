#include <iostream>
#include <fstream>
#include <string>
#include <cstdint>
#include <cctype>
#include <limits>
#include <cstdio>

static inline bool is_invalid(uint64_t x) {
    char s[32];
    int n = snprintf(s, sizeof(s), "%llu", (unsigned long long)x);
    if (n <= 1) return false;
    for (int p = 1; p <= n / 2; ++p) {
        if (n % p) continue;
        int k = n / p;
        if (k < 2) continue;
        bool ok = true;
        for (int i = p; i < n && ok; ++i) {
            if (s[i] != s[i % p]) ok = false;
        }
        if (ok) return true;
    }
    return false;
}

int main() {
    std::ifstream fin("input.txt", std::ios::binary);
    if (!fin) return 1;
    std::string content((std::istreambuf_iterator<char>(fin)), std::istreambuf_iterator<char>());
    fin.close();

    uint64_t sum = 0;
    size_t pos = 0;
    const size_t len = content.size();

    auto skip = [&]() {
        while (pos < len) {
            char c = content[pos];
            if (c == ' ' || c == '\n' || c == '\r' || c == '\t' || c == ',')
                ++pos;
            else
                break;
        }
    };

    while (true) {
        skip();
        if (pos >= len) break;

        if (!std::isdigit((unsigned char)content[pos])) break;
        uint64_t a = 0;
        while (pos < len && std::isdigit((unsigned char)content[pos])) {
            a = a * 10 + (content[pos] - '0');
            ++pos;
        }

        if (pos >= len || content[pos] != '-') break;
        ++pos;

        if (pos >= len || !std::isdigit((unsigned char)content[pos])) break;
        uint64_t b = 0;
        while (pos < len && std::isdigit((unsigned char)content[pos])) {
            b = b * 10 + (content[pos] - '0');
            ++pos;
        }

        if (a > b) { uint64_t t = a; a = b; b = t; }

        for (uint64_t x = a; x <= b; ++x) {
            if (is_invalid(x)) sum += x;
            if (x == std::numeric_limits<uint64_t>::max()) break;
        }
    }

    std::cout << sum << "\n";
    return 0;
}