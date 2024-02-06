
#include <iostream>
#include <fstream>
#include <vector>
#include <queue>

struct Position {
    int x, y, risk;
};

struct CompareRisk {
    bool operator()(const Position& a, const Position& b) {
        return a.risk > b.risk;
    }
};

std::vector<std::vector<int> > extendGrid(std::vector<std::vector<int> >& initialGrid) {
    int rows = initialGrid.size();
    int cols = initialGrid[0].size();
    std::vector<std::vector<int> > extendedGrid(rows * 5, std::vector<int>(cols * 5, 0));

    for (int i = 0; i < rows * 5; i++) {
        for (int j = 0; j < cols * 5; j++) {
            int newRisk = initialGrid[i % rows][j % cols] + i / rows + j / cols;
            if (newRisk > 9) {
                newRisk -= 9;
            }
            extendedGrid[i][j] = newRisk;
        }
    }

    return extendedGrid;
}

int dijkstra(std::vector<std::vector<int> >& grid) {
    std::priority_queue<Position, std::vector<Position>, CompareRisk> pq;
    pq.push({0, 0, 0});

    int rows = grid.size();
    int cols = grid[0].size();
    std::vector<std::vector<int> > dist(rows, std::vector<int>(cols, INT_MAX));
    dist[0][0] = 0;

    std::vector<Position> directions = {{1, 0, 0}, {0, 1, 0}, {-1, 0, 0}, {0, -1, 0}};

    while (!pq.empty()) {
        Position curr = pq.top();
        pq.pop();

        if (curr.x == rows - 1 && curr.y == cols - 1) {
            return curr.risk;
        }

        for (const auto& d : directions) {
            int nx = curr.x + d.x;
            int ny = curr.y + d.y;

            if (nx >= 0 && ny >= 0 && nx < rows && ny < cols) {
                int nextRisk = curr.risk + grid[nx][ny];
                if (nextRisk < dist[nx][ny]) {
                    dist[nx][ny] = nextRisk;
                    pq.push({nx, ny, nextRisk});
                }
            }
        }
    }

    return -1;
}

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cout << "Error opening file" << std::endl;
        return 1;
    }

    std::vector<std::vector<int> > initialGrid;
    std::string line;
    while (std::getline(file, line)) {
        std::vector<int> row;
        for (char ch : line) {
            row.push_back(ch - '0');
        }
        initialGrid.push_back(row);
    }

    file.close();

    std::vector<std::vector<int> > extendedGrid = extendGrid(initialGrid);

    std::cout << dijkstra(extendedGrid) << std::endl;

    return 0;
}
