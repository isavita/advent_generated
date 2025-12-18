#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <algorithm>

int main() {
    std::ifstream fin("input.txt");
    if (!fin) return 1;

    std::vector<std::string> grid;
    std::string line;
    int width = 0, height = 0;

    while (std::getline(fin, line)) {
        if (!line.empty() && line.back() == '\r') line.pop_back();
        grid.push_back(line);
        if (width == 0) width = (int)line.size();
        height++;
    }
    fin.close();

    int sx = 0, sy = 0;
    bool found = false;
    for (int y = 0; y < height && !found; ++y) {
        for (int x = 0; x < width && x < (int)grid[y].size(); ++x) {
            if (grid[y][x] == 'S') { sx = x; sy = y; found = true; break; }
        }
    }

    std::vector<char> active(width, 0), next(width, 0);
    int splits = 0;
    if (width > 0) active[sx] = 1;

    for (int y = sy; y < height; ++y) {
        std::fill(next.begin(), next.end(), 0);
        for (int x = 0; x < width; ++x) {
            if (!active[x]) continue;
            char c = (x < (int)grid[y].size()) ? grid[y][x] : ' ';
            if (c == '^') {
                ++splits;
                if (x - 1 >= 0) next[x - 1] = 1;
                if (x + 1 < width) next[x + 1] = 1;
            } else {
                next[x] = 1;
            }
        }
        active.swap(next);
        bool any = false;
        for (int i = 0; i < width; ++i) if (active[i]) { any = true; break; }
        if (!any) break;
    }

    std::cout << splits << "\n";
    return 0;
}