
#include <iostream>
#include <fstream>
#include <vector>
#include <map>

using namespace std;

struct State {
    int x, y, dir;
    bool operator<(const State& other) const {
        if (x != other.x) return x < other.x;
        if (y != other.y) return y < other.y;
        return dir < other.dir;
    }
};

bool loops(vector<string>& grid, int sx, int sy, int sdir) {
    int h = grid.size();
    int w = grid[0].size();
    int dirs[4][2] = {{0, -1}, {1, 0}, {0, 1}, {-1, 0}};
    int x = sx, y = sy, dir = sdir;
    map<State, bool> seen;
    for (int step = 0; step < 2000000; step++) {
        State st = {x, y, dir};
        if (seen[st]) {
            return true;
        }
        seen[st] = true;
        int dx = dirs[dir][0], dy = dirs[dir][1];
        int nx = x + dx, ny = y + dy;
        if (nx < 0 || nx >= w || ny < 0 || ny >= h) {
            return false;
        }
        if (grid[ny][nx] == '#') {
            dir = (dir + 1) % 4;
            continue;
        }
        x = nx;
        y = ny;
    }
    return false;
}

int main() {
    ifstream f("input.txt");
    if (!f.is_open()) {
        cerr << "Error opening file" << endl;
        return 1;
    }

    vector<string> grid;
    string line;
    while (getline(f, line)) {
        grid.push_back(line);
    }
    f.close();

    int h = grid.size();
    int w = grid[0].size();

    int startX, startY, startDir;
    for (int i = 0; i < h; i++) {
        for (int j = 0; j < w; j++) {
            switch (grid[i][j]) {
                case '^':
                    startX = j;
                    startY = i;
                    startDir = 0;
                    break;
                case '>':
                    startX = j;
                    startY = i;
                    startDir = 1;
                    break;
                case 'v':
                    startX = j;
                    startY = i;
                    startDir = 2;
                    break;
                case '<':
                    startX = j;
                    startY = i;
                    startDir = 3;
                    break;
            }
        }
    }
    grid[startY][startX] = '.';

    int canLoop = 0;
    for (int y = 0; y < h; y++) {
        for (int x = 0; x < w; x++) {
            if (x == startX && y == startY) {
                continue;
            }
            if (grid[y][x] != '.') {
                continue;
            }
            grid[y][x] = '#';
            if (loops(grid, startX, startY, startDir)) {
                canLoop++;
            }
            grid[y][x] = '.';
        }
    }

    cout << canLoop << endl;

    return 0;
}
