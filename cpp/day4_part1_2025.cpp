
#include <fstream>
#include <iostream>
#include <vector>
#include <string>

int main() {
    std::ifstream file("input.txt");
    if (!file) return 0;

    std::vector<std::string> grid;
    std::string line;
    while (std::getline(file, line)) grid.push_back(line);

    int rows = grid.size();
    int cols = grid[0].size();
    int count = 0;

    const int dx[8] = {-1,-1,-1,0,0,1,1,1};
    const int dy[8] = {-1,0,1,-1,1,-1,0,1};

    for (int i = 0; i < rows; ++i) {
        for (int j = 0; j < cols; ++j) {
            if (grid[i][j] != '@') continue;
            int neighbors = 0;
            for (int k = 0; k < 8; ++k) {
                int ni = i + dx[k];
                int nj = j + dy[k];
                if (ni >= 0 && ni < rows && nj >= 0 && nj < cols && grid[ni][nj] == '@') {
                    ++neighbors;
                }
            }
            if (neighbors < 4) ++count;
        }
    }

    std::cout << count << std::endl;
    return 0;
}
