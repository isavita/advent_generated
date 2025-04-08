
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <stdexcept> // For stoll exceptions if needed

using u128 = unsigned __int128;

const u128 SIZE = 119315717514047;
const u128 ITER = 101741582076661;
const u128 CARD_INDEX = 2020;

// Helper function to print u128
std::ostream& operator<<(std::ostream& os, u128 n) {
    if (n == 0) return os << "0";
    std::string s = "";
    while (n > 0) {
        s = std::to_string((int)(n % 10)) + s;
        n /= 10;
    }
    return os << s;
}

// Modular multiplication: (a * b) % m
u128 modmul(u128 a, u128 b, u128 m) {
    return (a * b) % m;
}

// Modular exponentiation: (base^exp) % m
u128 modpow(u128 base, u128 exp, u128 m) {
    u128 res = 1;
    base %= m;
    while (exp > 0) {
        if (exp % 2 == 1) res = modmul(res, base, m);
        base = modmul(base, base, m);
        exp /= 2;
    }
    return res;
}

// Modular multiplicative inverse: n^-1 % m (using Fermat's Little Theorem)
// Assumes m is prime
u128 modinv(u128 n, u128 m) {
    return modpow(n, m - 2, m);
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::ifstream file("input.txt");
    if (!file.is_open()) {
        // Error handling is explanation, skip as requested.
        // std::cerr << "Error opening input.txt" << std::endl;
        return 1;
    }

    u128 offset = 0;
    u128 increment = 1;
    std::string line;

    while (std::getline(file, line)) {
         // Minimal trim
        if (!line.empty() && line.back() == '\r') {
            line.pop_back();
        }
        if (line.empty()) continue;


        if (line == "deal into new stack") {
            increment = modmul(increment, SIZE - 1, SIZE);
            offset = (offset + increment) % SIZE;
        } else if (line.rfind("cut ", 0) == 0) {
            long long n_ll = std::stoll(line.substr(4));
            u128 n = (n_ll >= 0) ? n_ll : (SIZE - (u128)(-n_ll) % SIZE) % SIZE;
            offset = (offset + modmul(n, increment, SIZE)) % SIZE;
        } else if (line.rfind("deal with increment ", 0) == 0) {
            long long n_ll = std::stoll(line.substr(20));
            u128 n = n_ll;
            increment = modmul(increment, modinv(n, SIZE), SIZE);
        }
    }
    file.close();

    u128 final_increment;
    u128 final_offset;

    if (increment == 1) {
        final_increment = 1;
        final_offset = modmul(offset, ITER, SIZE);
    } else {
        final_increment = modpow(increment, ITER, SIZE);
        u128 term1 = (final_increment + SIZE - 1) % SIZE;
        u128 term2 = modinv((increment + SIZE - 1) % SIZE, SIZE);
        u128 term3 = modmul(offset, term1, SIZE);
        final_offset = modmul(term3, term2, SIZE);
    }

    u128 answer = (modmul(CARD_INDEX, final_increment, SIZE) + final_offset) % SIZE;

    std::cout << answer << std::endl;

    return 0;
}
