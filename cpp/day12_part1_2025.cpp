
#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <sstream>
#include <algorithm>
#include <map>
#include <set>

using namespace std;

struct Point {
    int r, c;
    bool operator<(const Point& o) const { return r < o.r || (r == o.r && c < o.c); }
    bool operator==(const Point& o) const { return r == o.r && c == o.c; }
};

typedef vector<Point> Piece;

Piece normalize(Piece p) {
    if (p.empty()) return p;
    int mR = 1e9, mC = 1e9;
    for (auto& pt : p) { mR = min(mR, pt.r); mC = min(mC, pt.c); }
    for (auto& pt : p) { pt.r -= mR; pt.c -= mC; }
    sort(p.begin(), p.end());
    return p;
}

Piece rotate(Piece p) {
    for (auto& pt : p) { int t = pt.r; pt.r = pt.c; pt.c = -t; }
    return normalize(p);
}

Piece flip(Piece p) {
    for (auto& pt : p) pt.c = -pt.c;
    return normalize(p);
}

vector<Piece> getVars(Piece p) {
    set<Piece> s;
    for (int i = 0; i < 4; i++) {
        p = rotate(p); s.insert(p);
        Piece f = flip(p); s.insert(normalize(f));
    }
    return vector<Piece>(s.begin(), s.end());
}

bool check(int R, int C, const vector<bool>& G, map<int, int>& counts, map<int, Piece>& shapes, const vector<int>& ids) {
    int minS = 1e9; bool has = 0;
    for (int id : ids) if (id != -1 && counts[id] > 0) { minS = min(minS, (int)shapes[id].size()); has = 1; }
    if (!has) return 1;
    int slack = (counts.count(-1) ? counts[-1] : 0);
    vector<bool> vis(R * C, 0);
    for (int i = 0; i < R * C; i++) {
        if (!G[i] && !vis[i]) {
            int sz = 0; vector<int> q = {i}; vis[i] = 1;
            for (int j = 0; j < (int)q.size(); j++) {
                sz++; int r = q[j] / C, c = q[j] % C;
                int dr[] = {0, 0, 1, -1}, dc[] = {1, -1, 0, 0};
                for (int d = 0; d < 4; d++) {
                    int nr = r + dr[d], nc = c + dc[d];
                    if (nr >= 0 && nr < R && nc >= 0 && nc < C && !G[nr * C + nc] && !vis[nr * C + nc]) {
                        vis[nr * C + nc] = 1; q.push_back(nr * C + nc);
                    }
                }
            }
            if (sz < minS) { if (slack >= sz) slack -= sz; else return 0; }
        }
    }
    return 1;
}

bool solve(int R, int C, vector<bool>& G, map<int, int>& counts, const vector<int>& ids, map<int, vector<Piece>>& vars, map<int, Piece>& shapes) {
    int idx = 0; while (idx < R * C && G[idx]) idx++;
    if (idx == R * C) return 1;
    if (!check(R, C, G, counts, shapes, ids)) return 0;
    int r = idx / C, c = idx % C;
    for (int id : ids) {
        if (counts[id] > 0) {
            counts[id]--;
            for (auto& p : vars[id]) {
                bool ok = 1;
                for (auto& pt : p) {
                    int nr = r + pt.r, nc = c + pt.c;
                    if (nr < 0 || nr >= R || nc < 0 || nc >= C || G[nr * C + nc]) { ok = 0; break; }
                }
                if (ok) {
                    for (auto& pt : p) G[(r + pt.r) * C + (c + pt.c)] = 1;
                    if (solve(R, C, G, counts, ids, vars, shapes)) return 1;
                    for (auto& pt : p) G[(r + pt.r) * C + (c + pt.c)] = 0;
                }
            }
            counts[id]++;
        }
    }
    return 0;
}

int main() {
    ifstream f("input.txt"); string l; map<int, Piece> sh; map<int, vector<Piece>> vr;
    vector<string> cs, rl; int ci = -1; bool ps = 1;
    auto pp = [&](vector<string>& lines) {
        Piece p; for (int r = 0; r < (int)lines.size(); r++)
            for (int c = 0; c < (int)lines[r].size(); c++) if (lines[r][c] == '#') p.push_back({r, c});
        return normalize(p);
    };
    while (getline(f, l)) {
        l.erase(0, l.find_first_not_of(" \t\r\n")); l.erase(l.find_last_not_of(" \t\r\n") + 1);
        if (l.empty()) continue;
        if (l.find('x') != string::npos && l.find(':') != string::npos) {
            if (ps && ci != -1) { Piece p = pp(cs); if (!p.empty()) { sh[ci] = p; vr[ci] = getVars(p); } ci = -1; }
            ps = 0;
        }
        if (ps) {
            if (l.back() == ':') {
                if (ci != -1) { Piece p = pp(cs); if (!p.empty()) { sh[ci] = p; vr[ci] = getVars(p); } }
                ci = stoi(l.substr(0, l.size() - 1)); cs.clear();
            } else cs.push_back(l);
        } else rl.push_back(l);
    }
    if (ci != -1) { Piece p = pp(cs); if (!p.empty()) { sh[ci] = p; vr[ci] = getVars(p); } }
    sh[-1] = {{0, 0}}; vr[-1] = {{{0, 0}}};
    int sol = 0;
    for (auto& r : rl) {
        size_t s = r.find(':'); if (s == string::npos) continue;
        string d = r.substr(0, s), cn = r.substr(s + 1);
        int W = stoi(d.substr(0, d.find('x'))), H = stoi(d.substr(d.find('x') + 1));
        stringstream ss(cn); int c, id = 0, a = 0; map<int, int> pc;
        while (ss >> c) { if (c > 0) { pc[id] = c; if (sh.count(id)) a += c * (int)sh[id].size(); } id++; }
        if (a > W * H) continue;
        if (W * H - a > 0) pc[-1] = W * H - a;
        vector<int> ids; for (auto const& [i, v] : pc) ids.push_back(i);
        sort(ids.begin(), ids.end(), [&](int x, int y) { return sh[x].size() > sh[y].size(); });
        vector<bool> G(W * H, 0); if (solve(H, W, G, pc, ids, vr, sh)) sol++;
    }
    cout << "Number of regions that fit all presents: " << sol << endl;
    return 0;
}
