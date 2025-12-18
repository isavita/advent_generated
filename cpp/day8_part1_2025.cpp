#include <iostream>
#include <vector>
#include <algorithm>
#include <numeric>
#include <fstream>
#include <string>
#include <cstdio>

struct Point { int x, y, z; };
struct Edge { int u, v; long long d; };

class UnionFind {
public:
    std::vector<int> parent, sz;
    UnionFind(int n) : parent(n), sz(n, 1) {
        std::iota(parent.begin(), parent.end(), 0);
    }
    int find(int x) {
        if (parent[x] == x) return x;
        return parent[x] = find(parent[x]);
    }
    void unite(int a, int b) {
        int ra = find(a), rb = find(b);
        if (ra == rb) return;
        if (sz[ra] < sz[rb]) std::swap(ra, rb);
        parent[rb] = ra;
        sz[ra] += sz[rb];
    }
    int root_size(int x) {
        int r = find(x);
        return sz[r];
    }
};

int main() {
    std::ifstream fin("input.txt");
    if (!fin) return 1;

    std::vector<Point> pts;
    pts.reserve(128);
    std::string line;
    while (std::getline(fin, line)) {
        int x, y, z;
        if (sscanf(line.c_str(), " %d , %d , %d", &x, &y, &z) != 3) continue;
        pts.push_back({x, y, z});
    }
    fin.close();

    int n = (int)pts.size();
    if (n < 2) {
        printf("Not enough points to form circuits.\n");
        return 0;
    }

    // Generate edges
    size_t e_cnt = (size_t)n * (n - 1) / 2;
    std::vector<Edge> edges;
    edges.reserve(e_cnt);
    for (int i = 0; i < n; ++i) {
        for (int j = i + 1; j < n; ++j) {
            long long dx = (long long)pts[i].x - pts[j].x;
            long long dy = (long long)pts[i].y - pts[j].y;
            long long dz = (long long)pts[i].z - pts[j].z;
            edges.push_back({i, j, dx*dx + dy*dy + dz*dz});
        }
    }

    std::sort(edges.begin(), edges.end(), [](const Edge& a, const Edge& b){
        return a.d < b.d;
    });

    // Union-Find
    UnionFind uf(n);
    size_t limit = std::min((size_t)1000, edges.size());
    for (size_t i = 0; i < limit; ++i) uf.unite(edges[i].u, edges[i].v);

    // Find three largest component sizes
    int top[3] = {0, 0, 0};
    for (int i = 0; i < n; ++i) {
        if (uf.find(i) == i) {
            int s = uf.root_size(i);
            if (s > top[0]) {
                top[2] = top[1];
                top[1] = top[0];
                top[0] = s;
            } else if (s > top[1]) {
                top[2] = top[1];
                top[1] = s;
            } else if (s > top[2]) {
                top[2] = s;
            }
        }
    }

    unsigned long long product = 1;
    for (int k = 0; k < 3 && top[k] > 0; ++k)
        product *= static_cast<unsigned long long>(top[k]);

    printf("Product of three largest circuit sizes: %llu\n", product);
    return 0;
}