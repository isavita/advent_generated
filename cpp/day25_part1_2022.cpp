
#include <iostream>
#include <fstream>
#include <string>
#include <algorithm>
#include <vector>

long long fromSnafu(const std::string& s) {
    long long n = 0;
    for (char c : s) {
        n *= 5;
        if (c == '=') {
            n -= 2;
        } else if (c == '-') {
            n -= 1;
        } else {
            n += c - '0';
        }
    }
    return n;
}

std::string toSnafu(long long n) {
    std::string b;
    while (n > 0) {
        int rem = n % 5;
        if (rem == 3) {
            n += 5;
            b += '=';
        } else if (rem == 4) {
            n += 5;
            b += '-';
        } else {
            b += '0' + rem;
        }
        n /= 5;
    }
    std::reverse(b.begin(), b.end());
    return b;
}

int main() {
    std::ifstream file("input.txt");
    std::string line;
    long long sum = 0;
    while (std::getline(file, line)) {
        sum += fromSnafu(line);
    }
    std::cout << toSnafu(sum) << std::endl;
    return 0;
}
