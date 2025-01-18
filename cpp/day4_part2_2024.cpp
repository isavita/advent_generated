
#include <iostream>
#include <fstream>
#include <vector>
#include <string>

using namespace std;

bool checkMAS(const vector<string>& grid, int x, int y, int dx, int dy) {
    int rows = grid.size();
    int cols = grid[0].size();
    if (x < 0 || y < 0 || x >= rows || y >= cols) return false;

    const string word = "MAS";
    bool forward = true;
    bool backward = true;

    for (int i = 0; i < 3; ++i) {
        int newX = x + dx * i;
        int newY = y + dy * i;
        if (newX < 0 || newY < 0 || newX >= rows || newY >= cols) {
            forward = false;
            break;
        }
        if (grid[newX][newY] != word[i]) {
            forward = false;
        }
    }

    for (int i = 0; i < 3; ++i) {
        int newX = x + dx * i;
        int newY = y + dy * i;
        if (newX < 0 || newY < 0 || newX >= rows || newY >= cols) {
            backward = false;
            break;
        }
        if (grid[newX][newY] != word[2 - i]) {
            backward = false;
        }
    }
    return forward || backward;
}

bool checkXMAS(const vector<string>& grid, int x, int y) {
    return (checkMAS(grid, x - 1, y - 1, 1, 1) && checkMAS(grid, x - 1, y + 1, 1, -1)) ||
           (checkMAS(grid, x + 1, y - 1, -1, 1) && checkMAS(grid, x + 1, y + 1, -1, -1));
}

int countXMASPatterns(const vector<string>& grid) {
    int count = 0;
    int rows = grid.size();
    if (rows < 3) return 0;
    int cols = grid[0].size();
    if (cols < 3) return 0;

    for (int i = 1; i < rows - 1; ++i) {
        for (int j = 1; j < cols - 1; ++j) {
            if (grid[i][j] == 'A' && checkXMAS(grid, i, j)) {
                count++;
            }
        }
    }
    return count;
}

int main() {
    ifstream file("input.txt");
    if (!file.is_open()) {
        cerr << "Error opening file" << endl;
        return 1;
    }

    vector<string> grid;
    string line;
    while (getline(file, line)) {
        if (!line.empty()) {
            grid.push_back(line);
        }
    }
    file.close();

    int count = countXMASPatterns(grid);
    cout << "X-MAS patterns appear " << count << " times in the word search" << endl;
    return 0;
}
