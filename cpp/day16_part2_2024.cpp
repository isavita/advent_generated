
#include <iostream>
#include <vector>
#include <string>
#include <queue>
#include <tuple>
#include <algorithm>
#include <fstream>
#include <limits>

const long long INF = std::numeric_limits<long long>::max() / 2;

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::ifstream inputFile("input.txt");
    std::vector<std::string> grid;
    std::string line;
    while (std::getline(inputFile, line)) {
        grid.push_back(line);
    }
    inputFile.close();

    int n = grid.size();
    int m = grid[0].length();

    int sx, sy, ex, ey;
    for (int r = 0; r < n; ++r) {
        for (int c = 0; c < m; ++c) {
            if (grid[r][c] == 'S') {
                sx = r;
                sy = c;
            } else if (grid[r][c] == 'E') {
                ex = r;
                ey = c;
            }
        }
    }

    int dx[] = {-1, 0, 1, 0};
    int dy[] = {0, 1, 0, -1};

    std::vector<std::vector<std::vector<long long>>> dist(n, std::vector<std::vector<long long>>(m, std::vector<long long>(4, INF)));
    
    std::priority_queue<std::tuple<long long, int, int, int>,
                        std::vector<std::tuple<long long, int, int, int>>,
                        std::greater<std::tuple<long long, int, int, int>>> pq;

    dist[sx][sy][1] = 0;
    pq.push({0, sx, sy, 1});

    while (!pq.empty()) {
        auto [cost, x, y, d] = pq.top();
        pq.pop();

        if (cost > dist[x][y][d]) {
            continue;
        }

        for (int i = 0; i < 2; ++i) {
            int ndir;
            if (i == 0) ndir = (d + 1) % 4;
            else ndir = (d + 3) % 4;

            long long nc = cost + 1000;
            if (nc < dist[x][y][ndir]) {
                dist[x][y][ndir] = nc;
                pq.push({nc, x, y, ndir});
            }
        }

        int nx = x + dx[d];
        int ny = y + dy[d];

        if (nx >= 0 && nx < n && ny >= 0 && ny < m && grid[nx][ny] != '#') {
            long long nc = cost + 1;
            if (nc < dist[nx][ny][d]) {
                dist[nx][ny][d] = nc;
                pq.push({nc, nx, ny, d});
            }
        }
    }

    long long best = INF;
    for (int i = 0; i < 4; ++i) {
        best = std::min(best, dist[ex][ey][i]);
    }

    std::vector<std::vector<bool>> used(n, std::vector<bool>(m, false));
    std::queue<std::tuple<int, int, int>> rev_q;
    std::vector<std::vector<std::vector<bool>>> vis(n, std::vector<std::vector<bool>>(m, std::vector<bool>(4, false)));

    for (int d = 0; d < 4; ++d) {
        if (dist[ex][ey][d] == best) {
            if (!vis[ex][ey][d]) {
                rev_q.push({ex, ey, d});
                vis[ex][ey][d] = true;
            }
        }
    }

    while (!rev_q.empty()) {
        auto [x, y, d] = rev_q.front();
        rev_q.pop();

        used[x][y] = true;
        long long costU = dist[x][y][d];

        for (int i = 0; i < 2; ++i) {
            int pd;
            if (i == 0) pd = (d + 3) % 4;
            else pd = (d + 1) % 4;

            if (dist[x][y][pd] == costU - 1000 && !vis[x][y][pd]) {
                vis[x][y][pd] = true;
                rev_q.push({x, y, pd});
            }
        }

        int px = x - dx[d];
        int py = y - dy[d];

        if (px >= 0 && px < n && py >= 0 && py < m && grid[px][py] != '#') {
            if (dist[px][py][d] == costU - 1 && !vis[px][py][d]) {
                vis[px][py][d] = true;
                rev_q.push({px, py, d});
            }
        }
    }

    int count = 0;
    for (int r = 0; r < n; ++r) {
        for (int c = 0; c < m; ++c) {
            if (used[r][c]) {
                count++;
            }
        }
    }

    std::cout << count << std::endl;

    return 0;
}
