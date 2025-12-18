
#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>
#include <cstdio>
#include <string>

using namespace std;

struct Point {
    int x, y, z;
};

struct Edge {
    int u, v;
    long long d;
};

long long distSq(const Point& a, const Point& b) {
    long long dx = a.x - b.x;
    long long dy = a.y - b.y;
    long long dz = a.z - b.z;
    return dx*dx + dy*dy + dz*dz;
}

int find(vector<int>& parent, int x) {
    while (parent[x] != x) {
        parent[x] = parent[parent[x]];
        x = parent[x];
    }
    return x;
}

void unite(vector<int>& parent, vector<int>& rank, int a, int b) {
    a = find(parent, a);
    b = find(parent, b);
    if (a == b) return;
    if (rank[a] < rank[b]) {
        parent[a] = b;
    } else if (rank[a] > rank[b]) {
        parent[b] = a;
    } else {
        parent[b] = a;
        rank[a]++;
    }
}

int main() {
    ifstream file("input.txt");
    if (!file) {
        cerr << "Cannot open file." << endl;
        return 1;
    }

    vector<Point> pts;
    string line;
    while (getline(file, line)) {
        if (line.find_first_not_of(" \r\n") == string::npos) continue;
        int x, y, z;
        char ch;
        if (sscanf(line.c_str(), "%d,%d,%d", &x, &y, &z) == 3) {
            pts.push_back({x, y, z});
        }
    }
    file.close();

    size_t n = pts.size();
    if (n < 2) {
        return 0;
    }

    vector<Edge> edges;
    edges.reserve(n*(n-1)/2);
    for (size_t i = 0; i < n; ++i) {
        for (size_t j = i+1; j < n; ++j) {
            edges.push_back({ (int)i, (int)j, distSq(pts[i], pts[j]) });
        }
    }

    sort(edges.begin(), edges.end(), [](const Edge& a, const Edge& b) {
        return a.d < b.d;
    });

    vector<int> parent(n);
    vector<int> rank(n, 0);
    for (int i = 0; i < n; ++i) {
        parent[i] = i;
    }

    size_t comps = n;
    for (const Edge& edge : edges) {
        int u = edge.u;
        int v = edge.v;
        int ru = find(parent, u);
        int rv = find(parent, v);
        if (ru != rv) {
            unite(parent, rank, ru, rv);
            comps--;
            if (comps == 1) {
                Point p1 = pts[u], p2 = pts[v];
                cout << "Connected ";
                cout << p1.x << "," << p1.y << "," << p1.z << " and ";
                cout << p2.x << "," << p2.y << "," << p2.z << endl;
                cout << "Product of X coordinates: " << static_cast<long long>(p1.x) * p2.x << endl;
                break;
            }
        }
    }

    return 0;
}
