#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <unordered_map>

int main() {
    std::ifstream in("input.txt");
    if (!in) {
        perror("Error opening file");
        return 1;
    }

    std::vector<std::string> grid;
    std::string line;
    while (std::getline(in, line)) {
        if (!line.empty()) {
            if (!line.empty() && line.back() == '\r') line.pop_back();
            grid.push_back(line);
        }
    }
    in.close();

    int height = (int)grid.size();
    if (height == 0) {
        std::cout << 0 << std::endl;
        return 0;
    }

    int width = (int)grid[0].size();

    int startX = -1, startY = -1;
    for (int y = 0; y < height && startX == -1; ++y) {
        for (int x = 0; x < width; ++x) {
            if (grid[y][x] == 'S') {
                startX = x;
                startY = y;
                break;
            }
        }
    }

    if (startX == -1) {
        std::cerr << "Start point 'S' not found\n";
        return 1;
    }

    std::unordered_map<int, unsigned long long> counts;
    counts[startX] = 1ULL;

    for (int y = startY; y < height; ++y) {
        std::unordered_map<int, unsigned long long> next;
        next.reserve(counts.size() * 2);
        for (const auto &p : counts) {
            int x = p.first;
            unsigned long long cnt = p.second;
            bool isSplitter = (x >= 0 && x < width && grid[y][x] == '^');
            if (isSplitter) {
                int ks[2] = {x - 1, x + 1};
                for (int k = 0; k < 2; ++k) {
                    next[ks[k]] += cnt;
                }
            } else {
                next[x] += cnt;
            }
        }
        counts.swap(next);
    }

    unsigned long long total = 0;
    for (const auto &p : counts) total += p.second;
    std::cout << total << std::endl;
    return 0;
}