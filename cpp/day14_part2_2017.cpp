
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <numeric>
#include <algorithm>
#include <deque>

std::string knot_hash(const std::string& input) {
    std::vector<int> lengths;
    for (char c : input) {
        lengths.push_back(static_cast<int>(static_cast<unsigned char>(c)));
    }
    lengths.insert(lengths.end(), {17, 31, 73, 47, 23});

    std::vector<int> lst(256);
    std::iota(lst.begin(), lst.end(), 0);

    int pos = 0;
    int skip = 0;

    for (int round = 0; round < 64; ++round) {
        for (int length : lengths) {
            for (int i = 0; i < length / 2; ++i) {
                int start_idx = (pos + i) % 256;
                int end_idx = (pos + length - 1 - i) % 256;
                std::swap(lst[start_idx], lst[end_idx]);
            }
            pos = (pos + length + skip) % 256;
            skip++;
        }
    }

    std::string binary_hash = "";
    binary_hash.reserve(128); // Pre-allocate memory for 128 characters
    for (int i = 0; i < 256; i += 16) {
        int xor_sum = 0;
        for (int j = 0; j < 16; ++j) {
            xor_sum ^= lst[i + j];
        }
        for (int k = 7; k >= 0; --k) {
            binary_hash += ((xor_sum >> k) & 1) ? '1' : '0';
        }
    }
    return binary_hash;
}

void mark_region(std::vector<std::vector<char>>& grid, int r, int c) {
    std::deque<std::pair<int, int>> s; // Using deque as a stack for DFS
    s.push_back({r, c});
    grid[r][c] = '0';

    const int N = 128;
    int dr[] = {-1, 1, 0, 0}; // Row offsets: up, down, left, right
    int dc[] = {0, 0, -1, 1}; // Col offsets

    while (!s.empty()) {
        std::pair<int, int> current = s.back();
        s.pop_back();
        int curr_r = current.first;
        int curr_c = current.second;

        for (int i = 0; i < 4; ++i) {
            int nr = curr_r + dr[i];
            int nc = curr_c + dc[i];

            if (nr >= 0 && nr < N && nc >= 0 && nc < N && grid[nr][nc] == '1') {
                grid[nr][nc] = '0';
                s.push_back({nr, nc});
            }
        }
    }
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::ifstream file("input.txt");
    std::string key;
    std::getline(file, key);
    file.close();

    int used_squares = 0;
    std::vector<std::vector<char>> grid(128, std::vector<char>(128));

    for (int i = 0; i < 128; ++i) {
        std::string row_key = key + "-" + std::to_string(i);
        std::string hash_val = knot_hash(row_key);
        
        for (int j = 0; j < 128; ++j) {
            if (hash_val[j] == '1') {
                used_squares++;
            }
            grid[i][j] = hash_val[j];
        }
    }
    std::cout << used_squares << std::endl;

    int regions = 0;
    for (int i = 0; i < 128; ++i) {
        for (int j = 0; j < 128; ++j) {
            if (grid[i][j] == '1') {
                mark_region(grid, i, j);
                regions++;
            }
        }
    }
    std::cout << regions << std::endl;

    return 0;
}
