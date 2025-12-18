#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <algorithm>

int main() {
    std::ifstream fin("input.txt");
    if (!fin) return 0;

    std::vector<std::string> grid;
    std::string line;
    while (std::getline(fin, line)) {
        if (!line.empty() && line.back() == '\r') line.pop_back();
        grid.push_back(line);
    }

    int R = (int)grid.size();
    int C = 0;
    for (const auto &s : grid) C = std::max(C, (int)s.size());

    if (R == 0 || C == 0) {
        std::cout << "Total rolls removed: 0\n";
        return 0;
    }

    int removed = 0;
    const int dr[8] = {-1,-1,-1,0,0,1,1,1};
    const int dc[8] = {-1,0,1,-1,1,-1,0,1};

    auto getChar = [&](int r, int c) -> char {
        if (r < 0 || r >= R || c < 0) return 0;
        if (c >= (int)grid[r].size()) return 0;
        return grid[r][c];
    };

    bool changed;
    do {
        changed = false;
        for (int r = 0; r < R; ++r) {
            for (int c = 0; c < C; ++c) {
                if (getChar(r, c) != '@') continue;
                int cnt = 0;
                for (int k = 0; k < 8; ++k) {
                    int nr = r + dr[k], nc = c + dc[k];
                    if (getChar(nr, nc) == '@') ++cnt;
                }
                if (cnt < 4) {
                    if (c < (int)grid[r].size()) grid[r][c] = '*';
                    changed = true;
                }
            }
        }
        for (int r = 0; r < R; ++r) {
            for (int c = 0; c < C; ++c) {
                if (getChar(r, c) == '*') {
                    if (c < (int)grid[r].size()) grid[r][c] = '.';
                    ++removed;
                }
            }
        }
    } while (changed);

    std::cout << "Total rolls removed: " << removed << "\n";
    return 0;
}