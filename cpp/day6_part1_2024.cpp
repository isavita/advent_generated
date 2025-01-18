
#include <iostream>
#include <fstream>
#include <vector>
#include <unordered_set>

using namespace std;

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
    int x, y, dirX, dirY, dirIdx;
    bool found = false;
    int dirs[4][2] = {{0, -1}, {1, 0}, {0, 1}, {-1, 0}};

    for (int i = 0; i < h && !found; ++i) {
        for (int j = 0; j < w && !found; ++j) {
            switch (grid[i][j]) {
                case '^':
                    x = j; y = i; dirIdx = 0;
                    found = true;
                    break;
                case '>':
                    x = j; y = i; dirIdx = 1;
                    found = true;
                    break;
                case 'v':
                    x = j; y = i; dirIdx = 2;
                    found = true;
                    break;
                case '<':
                    x = j; y = i; dirIdx = 3;
                    found = true;
                    break;
            }
        }
    }
    dirX = dirs[dirIdx][0];
    dirY = dirs[dirIdx][1];

    unordered_set<long long> visited;
    visited.insert(((long long)x << 32) | y);

    while (true) {
        int nx = x + dirX;
        int ny = y + dirY;
        if (nx < 0 || nx >= w || ny < 0 || ny >= h) {
            break;
        }
        if (grid[ny][nx] == '#') {
            dirIdx = (dirIdx + 1) % 4;
            dirX = dirs[dirIdx][0];
            dirY = dirs[dirIdx][1];
            continue;
        }
        x = nx;
        y = ny;
        visited.insert(((long long)x << 32) | y);
    }

    cout << visited.size() << endl;

    return 0;
}
