
#include <iostream>
#include <vector>
#include <string>
#include <queue>
#include <fstream>

struct Pos {
    int r, c;
};

std::vector<std::vector<int>> bfs(Pos start, int h, int w, const std::vector<std::vector<bool>>& walls) {
    std::vector<std::vector<int>> dist(h, std::vector<int>(w, -1));
    std::deque<Pos> q;

    dist[start.r][start.c] = 0;
    q.push_back(start);

    int dr[] = {1, -1, 0, 0};
    int dc[] = {0, 0, 1, -1};

    while (!q.empty()) {
        Pos curr = q.front();
        q.pop_front();

        for (int i = 0; i < 4; ++i) {
            int nr = curr.r + dr[i];
            int nc = curr.c + dc[i];

            if (nr >= 0 && nr < h && nc >= 0 && nc < w) {
                if (walls[nr][nc]) {
                    continue;
                }
                if (dist[nr][nc] == -1) {
                    dist[nr][nc] = dist[curr.r][curr.c] + 1;
                    q.push_back({nr, nc});
                }
            }
        }
    }
    return dist;
}

bool isTrack(int r, int c, int h, int w, const std::vector<std::vector<bool>>& walls) {
    return r >= 0 && r < h && c >= 0 && c < w && !walls[r][c];
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::vector<std::string> grid;
    std::string line;
    std::ifstream inputFile("input.txt");

    if (!inputFile.is_open()) {
        return 1;
    }

    while (std::getline(inputFile, line)) {
        grid.push_back(line);
    }
    inputFile.close();

    int h = grid.size();
    if (h == 0) {
        std::cout << 0 << std::endl;
        return 0;
    }
    int w = grid[0].length();
    if (w == 0) {
        std::cout << 0 << std::endl;
        return 0;
    }

    Pos S, E;
    std::vector<std::vector<bool>> walls(h, std::vector<bool>(w, false));
    std::vector<Pos> trackCells;

    for (int r = 0; r < h; ++r) {
        for (int c = 0; c < w; ++c) {
            if (grid[r][c] == 'S') {
                S = {r, c};
            } else if (grid[r][c] == 'E') {
                E = {r, c};
            }
            if (grid[r][c] == '#') {
                walls[r][c] = true;
            } else {
                trackCells.push_back({r, c});
            }
        }
    }

    std::vector<std::vector<int>> distFromS = bfs(S, h, w, walls);
    std::vector<std::vector<int>> distFromE = bfs(E, h, w, walls);

    if (distFromS[E.r][E.c] == -1) {
        std::cout << 0 << std::endl;
        return 0;
    }
    int normalCost = distFromS[E.r][E.c];

    int possibleCheats = 0;
    int dr_dir[] = {1, -1, 0, 0};
    int dc_dir[] = {0, 0, 1, -1};

    for (const auto& startPos : trackCells) {
        int sd = distFromS[startPos.r][startPos.c];
        if (sd == -1) {
            continue;
        }

        for (int i = 0; i < 4; ++i) {
            int m1r = startPos.r + dr_dir[i];
            int m1c = startPos.c + dc_dir[i];

            if (!(m1r >= 0 && m1r < h && m1c >= 0 && m1c < w)) {
                continue;
            }

            for (int j = 0; j < 4; ++j) {
                int m2r = m1r + dr_dir[j];
                int m2c = m1c + dc_dir[j];

                if (!isTrack(m2r, m2c, h, w, walls)) {
                    continue;
                }
                
                int ed = distFromE[m2r][m2c];
                if (ed == -1) {
                    continue;
                }
                
                int newCost = sd + 2 + ed;
                int saving = normalCost - newCost;

                if (saving >= 100) {
                    possibleCheats++;
                }
            }
        }
    }
    std::cout << possibleCheats << std::endl;

    return 0;
}
