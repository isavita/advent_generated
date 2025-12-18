#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <sstream>
#include <algorithm>
#include <unordered_map>
#include <queue>

struct Point { int x, y; };

int main() {
    std::ifstream fin("input.txt");
    if (!fin) return 1;

    std::vector<Point> pts;
    std::vector<int> xs, ys;

    std::string line;
    while (std::getline(fin, line)) {
        for (char &c : line) if (c == ',') c = ' ';
        std::stringstream ss(line);
        int x, y;
        if (ss >> x >> y) {
            pts.push_back({x, y});
            xs.push_back(x);
            ys.push_back(y);
        }
    }
    fin.close();

    if (pts.empty()) {
        std::cout << "Largest valid area: 0\n";
        return 0;
    }

    std::sort(xs.begin(), xs.end());
    xs.erase(std::unique(xs.begin(), xs.end()), xs.end());
    std::sort(ys.begin(), ys.end());
    ys.erase(std::unique(ys.begin(), ys.end()), ys.end());

    int ux = (int)xs.size();
    int uy = (int)ys.size();

    std::unordered_map<int,int> xidx, yidx;
    xidx.reserve(ux * 2);
    yidx.reserve(uy * 2);
    for (int i = 0; i < ux; ++i) xidx[xs[i]] = i;
    for (int i = 0; i < uy; ++i) yidx[ys[i]] = i;

    int W = 2 * ux + 1;
    int H = 2 * uy + 1;

    std::vector<long long> colW(W, 0), rowH(H, 0);
    colW[0] = 1;
    for (int i = 0; i < ux; ++i) {
        colW[2 * i + 1] = 1;
        if (i + 1 < ux) {
            long long gap = (long long)xs[i + 1] - xs[i] - 1;
            if (gap < 0) gap = 0;
            colW[2 * i + 2] = gap;
        } else {
            colW[2 * i + 2] = 1;
        }
    }
    rowH[0] = 1;
    for (int i = 0; i < uy; ++i) {
        rowH[2 * i + 1] = 1;
        if (i + 1 < uy) {
            long long gap = (long long)ys[i + 1] - ys[i] - 1;
            if (gap < 0) gap = 0;
            rowH[2 * i + 2] = gap;
        } else {
            rowH[2 * i + 2] = 1;
        }
    }

    std::vector<std::vector<char>> grid(H, std::vector<char>(W, 0));

    int n = (int)pts.size();
    for (int i = 0; i < n; ++i) {
        Point a = pts[i];
        Point b = pts[(i + 1) % n];
        int gx1 = 2 * xidx[a.x] + 1;
        int gy1 = 2 * yidx[a.y] + 1;
        int gx2 = 2 * xidx[b.x] + 1;
        int gy2 = 2 * yidx[b.y] + 1;

        if (gx1 == gx2) {
            int y0 = std::min(gy1, gy2);
            int y1 = std::max(gy1, gy2);
            for (int y = y0; y <= y1; ++y) if (rowH[y] > 0) grid[y][gx1] = 1;
        } else {
            int x0 = std::min(gx1, gx2);
            int x1 = std::max(gx1, gx2);
            for (int x = x0; x <= x1; ++x) if (colW[x] > 0) grid[gy1][x] = 1;
        }
    }

    std::queue<std::pair<int,int>> q;
    grid[0][0] = 2;
    q.push({0, 0});
    const int dirs[4][2] = {{0,1},{0,-1},{1,0},{-1,0}};
    while (!q.empty()) {
        auto cur = q.front(); q.pop();
        int cx = cur.first, cy = cur.second;
        for (int d = 0; d < 4; ++d) {
            int nx = cx + dirs[d][0];
            int ny = cy + dirs[d][1];
            if (nx >= 0 && nx < W && ny >= 0 && ny < H && grid[ny][nx] == 0) {
                grid[ny][nx] = 2;
                q.push({nx, ny});
            }
        }
    }

    std::vector<std::vector<long long>> P(H, std::vector<long long>(W, 0));
    for (int y = 0; y < H; ++y) {
        long long rowSum = 0;
        for (int x = 0; x < W; ++x) {
            long long val = (grid[y][x] != 2) ? colW[x] * rowH[y] : 0;
            rowSum += val;
            long long above = (y > 0) ? P[y - 1][x] : 0;
            P[y][x] = rowSum + above;
        }
    }

    long long maxArea = 0;
    for (int i = 0; i < n; ++i) {
        for (int j = i; j < n; ++j) {
            long long w = (long long)std::llabs((long long)pts[i].x - pts[j].x) + 1;
            long long h = (long long)std::llabs((long long)pts[i].y - pts[j].y) + 1;
            long long area = w * h;
            if (area <= maxArea) continue;

            int gx1 = 2 * xidx[pts[i].x] + 1;
            int gy1 = 2 * yidx[pts[i].y] + 1;
            int gx2 = 2 * xidx[pts[j].x] + 1;
            int gy2 = 2 * yidx[pts[j].y] + 1;
            if (gx1 > gx2) std::swap(gx1, gx2);
            if (gy1 > gy2) std::swap(gy1, gy2);

            long long total = P[gy2][gx2];
            long long left = (gx1 ? P[gy2][gx1 - 1] : 0);
            long long up = (gy1 ? P[gy1 - 1][gx2] : 0);
            long long diag = (gx1 && gy1) ? P[gy1 - 1][gx1 - 1] : 0;
            long long valid = total - left - up + diag;

            if (valid == area) maxArea = area;
        }
    }

    std::cout << "Largest valid area: " << maxArea << "\n";
    return 0;
}