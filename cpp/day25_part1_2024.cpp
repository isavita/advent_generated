
#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <array>

std::array<int, 5> parse_lock(const std::vector<std::string>& b) {
    std::array<int, 5> h{};
    for (int c = 0; c < 5; ++c) {
        for (int r = 1; r < 7; ++r) {
            if (b[r][c] == '#') {
                h[c]++;
            } else {
                break;
            }
        }
    }
    return h;
}

std::array<int, 5> parse_key(const std::vector<std::string>& b) {
    std::array<int, 5> h{};
    for (int c = 0; c < 5; ++c) {
        for (int r = 5; r >= 0; --r) {
            if (b[r][c] == '#') {
                h[c]++;
            } else {
                break;
            }
        }
    }
    return h;
}

bool fits(const std::array<int, 5>& lock, const std::array<int, 5>& key) {
    for (int i = 0; i < 5; ++i) {
        if (lock[i] + key[i] > 5) {
            return false;
        }
    }
    return true;
}

void solve() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cerr << "Error opening input.txt" << std::endl;
        return;
    }

    std::vector<std::string> raw;
    std::string line;
    while (std::getline(file, line)) {
        if (!line.empty()) {
            raw.push_back(line);
        }
    }
    file.close();

    if (raw.size() % 7 != 0) {
        std::cout << 0 << std::endl;
        return;
    }

    std::vector<std::array<int, 5>> locks;
    std::vector<std::array<int, 5>> keys;

    for (size_t i = 0; i < raw.size(); i += 7) {
        std::vector<std::string> block(raw.begin() + i, raw.begin() + i + 7);

        bool skip_block = false;
        for (const auto& ln : block) {
            if (ln.length() < 5) {
                skip_block = true;
                break;
            }
        }
        if (skip_block) {
            continue;
        }

        bool is_lock = true;
        for (int c = 0; c < 5; ++c) {
            if (block[0][c] != '#') {
                is_lock = false;
                break;
            }
        }

        if (is_lock) {
            locks.push_back(parse_lock(block));
        } else {
            keys.push_back(parse_key(block));
        }
    }

    int count = 0;
    for (const auto& lock : locks) {
        for (const auto& key : keys) {
            if (fits(lock, key)) {
                count++;
            }
        }
    }

    std::cout << count << std::endl;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    solve();
    return 0;
}
