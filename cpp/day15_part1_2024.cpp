
#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <sstream>

using namespace std;

bool pushBoxes(vector<string>& grid, int r, int c, int dr, int dc) {
    int nr = r + dr;
    int nc = c + dc;
    if (grid[nr][nc] == '#') return false;
    if (grid[nr][nc] == 'O') {
        if (!pushBoxes(grid, nr, nc, dr, dc)) return false;
    }
    if (grid[nr][nc] == '.') {
        grid[nr][nc] = 'O';
        grid[r][c] = '.';
        return true;
    }
    return false;
}

int main() {
    ifstream file("input.txt");
    if (!file.is_open()) {
        cerr << "Error opening file" << endl;
        return 1;
    }

    vector<string> grid;
    string line;
    stringstream movesBuffer;
    bool readingMap = true;

    while (getline(file, line)) {
        if (readingMap) {
            if (line.find('#') != string::npos) {
                grid.push_back(line);
            } else {
                readingMap = false;
                movesBuffer << line;
            }
        } else {
            movesBuffer << line;
        }
    }

    string moves = movesBuffer.str();

    int robotR, robotC;
    for (int r = 0; r < grid.size(); ++r) {
        for (int c = 0; c < grid[r].size(); ++c) {
            if (grid[r][c] == '@') {
                robotR = r;
                robotC = c;
                break;
            }
        }
    }

    int dirs[4][2] = {{-1, 0}, {1, 0}, {0, -1}, {0, 1}};
    char dirChars[4] = {'^', 'v', '<', '>'};

    for (char move : moves) {
        int dIndex = 0;
        for (; dIndex < 4; ++dIndex) {
            if (dirChars[dIndex] == move) break;
        }
        int dr = dirs[dIndex][0];
        int dc = dirs[dIndex][1];
        int nr = robotR + dr;
        int nc = robotC + dc;

        if (grid[nr][nc] == '#') continue;
        if (grid[nr][nc] == 'O') {
            if (!pushBoxes(grid, nr, nc, dr, dc)) continue;
        }
        if (grid[nr][nc] == '.' || grid[nr][nc] == 'O') {
            grid[robotR][robotC] = '.';
            grid[nr][nc] = '@';
            robotR = nr;
            robotC = nc;
        }
    }

    int sum = 0;
    for (int r = 0; r < grid.size(); ++r) {
        for (int c = 0; c < grid[r].size(); ++c) {
            if (grid[r][c] == 'O') {
                sum += r * 100 + c;
            }
        }
    }

    cout << sum << endl;

    return 0;
}
