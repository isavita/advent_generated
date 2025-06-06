
#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <algorithm>
#include <cstdio>
#include <tuple>

using namespace std;

int main() {
    ios_base::sync_with_stdio(false);
    cin.tie(NULL);

    ifstream file("input.txt");
    string line;

    vector<tuple<char, int, int, int>> veins;
    int min_x = 501, max_x = 499, min_y = 1000000, max_y = 0;

    while (getline(file, line)) {
        char type = line[0];
        int c1, c2, c3;
        if (type == 'x') {
            sscanf(line.c_str(), "x=%d, y=%d..%d", &c1, &c2, &c3);
            min_x = min(min_x, c1);
            max_x = max(max_x, c1);
            min_y = min(min_y, c2);
            max_y = max(max_y, c3);
        } else {
            sscanf(line.c_str(), "y=%d, x=%d..%d", &c1, &c2, &c3);
            min_y = min(min_y, c1);
            max_y = max(max_y, c1);
            min_x = min(min_x, c2);
            max_x = max(max_x, c3);
        }
        veins.emplace_back(type, c1, c2, c3);
    }

    min_x--;
    max_x++;

    int offset_x = min_x;
    vector<vector<char>> grid(max_y + 2, vector<char>(max_x - min_x + 1, '.'));

    for (const auto& v : veins) {
        char type = get<0>(v);
        int c1 = get<1>(v), c2 = get<2>(v), c3 = get<3>(v);
        if (type == 'x') {
            for (int y = c2; y <= c3; ++y) {
                grid[y][c1 - offset_x] = '#';
            }
        } else {
            for (int x = c2; x <= c3; ++x) {
                grid[c1][x - offset_x] = '#';
            }
        }
    }

    vector<pair<int, int>> stack;
    stack.push_back({500 - offset_x, 0});

    while (!stack.empty()) {
        auto [x, y] = stack.back();
        stack.pop_back();

        while (y < max_y && grid[y + 1][x] == '.') {
            grid[y][x] = '|';
            y++;
        }

        if (y >= max_y || grid[y + 1][x] == '|') {
            grid[y][x] = '|';
            continue;
        }

        while (true) {
            int l = x, r = x;
            bool leak_left = false, leak_right = false;

            while (grid[y][l - 1] != '#' && (grid[y + 1][l - 1] == '#' || grid[y + 1][l - 1] == '~')) {
                l--;
            }
            if (grid[y + 1][l - 1] != '#' && grid[y + 1][l - 1] != '~') {
                leak_left = true;
            }

            while (grid[y][r + 1] != '#' && (grid[y + 1][r + 1] == '#' || grid[y + 1][r + 1] == '~')) {
                r++;
            }
            if (grid[y + 1][r + 1] != '#' && grid[y + 1][r + 1] != '~') {
                leak_right = true;
            }

            char fill = (leak_left || leak_right) ? '|' : '~';

            for (int i = l; i <= r; ++i) {
                grid[y][i] = fill;
            }

            if (fill == '|') {
                if (leak_left) stack.push_back({l - 1, y});
                if (leak_right) stack.push_back({r + 1, y});
                break;
            }
            y--;
        }
    }

    int count = 0;
    for (int r = min_y; r <= max_y; ++r) {
        for (char cell : grid[r]) {
            if (cell == '~' || cell == '|') {
                count++;
            }
        }
    }

    cout << count << endl;

    return 0;
}
