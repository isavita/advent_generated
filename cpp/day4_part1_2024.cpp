
#include <iostream>
#include <fstream>
#include <vector>
#include <string>

using namespace std;

struct Direction {
    int dx, dy;
};

const Direction directions[] = {
    {0, 1}, {1, 0}, {1, 1}, {-1, 1}, {0, -1}, {-1, 0}, {-1, -1}, {1, -1}
};

bool checkWord(const vector<string>& grid, const string& word, int x, int y, const Direction& d) {
    int rows = grid.size();
    int cols = rows > 0 ? grid[0].size() : 0;
    
    if (x < 0 || y < 0 || x >= rows || y >= cols) return false;

    for (size_t i = 0; i < word.size(); ++i) {
        int newX = x + d.dx * i;
        int newY = y + d.dy * i;
        if (newX < 0 || newY < 0 || newX >= rows || newY >= cols || grid[newX][newY] != word[i]) {
            return false;
        }
    }
    return true;
}

int countOccurrences(const vector<string>& grid, const string& word) {
    int count = 0;
    int rows = grid.size();
    if (rows == 0) return 0;
    int cols = grid[0].size();

    for (int i = 0; i < rows; ++i) {
        for (int j = 0; j < cols; ++j) {
            for (const auto& dir : directions) {
                if (checkWord(grid, word, i, j, dir)) {
                    count++;
                }
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

    int count = countOccurrences(grid, "XMAS");
    cout << "XMAS appears " << count << " times in the word search" << endl;

    return 0;
}
