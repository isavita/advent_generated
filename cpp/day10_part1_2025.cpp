
#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <sstream>
#include <algorithm>
#include <bitset>

using namespace std;

const int M = 1024;

struct Machine {
    vector<int> target;
    vector<vector<int>> buttons;
};

int solveMachine(Machine& m) {
    int R = m.target.size(), C = m.buttons.size();
    vector<bitset<M>> mat(R);
    for (int i = 0; i < R; i++) {
        if (m.target[i]) mat[i].set(C);
        for (int j = 0; j < C; j++)
            for (int l : m.buttons[j]) if (l == i) mat[i].set(j);
    }

    int pr = 0;
    vector<int> pc(R, -1);
    vector<bool> isP(C, 0);
    for (int c = 0; c < C && pr < R; c++) {
        int sel = -1;
        for (int r = pr; r < R; r++) if (mat[r].test(c)) { sel = r; break; }
        if (sel == -1) continue;
        swap(mat[pr], mat[sel]);
        for (int r = 0; r < R; r++) if (r != pr && mat[r].test(c)) mat[r] ^= mat[pr];
        pc[pr] = c; isP[c] = 1; pr++;
    }

    for (int r = pr; r < R; r++) if (mat[r].test(C)) return -1;

    vector<int> fv;
    for (int c = 0; c < C; c++) if (!isP[c]) fv.push_back(c);

    int mw = 1e9, nf = fv.size();
    for (long long i = 0; i < (1LL << nf); i++) {
        bitset<M> x;
        for (int j = 0; j < nf; j++) if ((i >> j) & 1) x.set(fv[j]);
        int cw = x.count();
        for (int r = 0; r < pr; r++) if (mat[r].test(C) ^ ((mat[r] & x).count() % 2)) cw++;
        mw = min(mw, cw);
    }
    return (mw == 1e9) ? -1 : mw;
}

int main() {
    ifstream f("input.txt");
    string l;
    long long t = 0;
    while (getline(f, l)) {
        size_t s = l.find('['), e = l.find(']');
        if (s == string::npos) continue;
        Machine m;
        string d = l.substr(s + 1, e - s - 1);
        for (char c : d) m.target.push_back(c == '#' ? 1 : 0);
        size_t p = e;
        while ((s = l.find('(', p)) != string::npos) {
            e = l.find(')', s);
            if (e == string::npos) break;
            stringstream ss(l.substr(s + 1, e - s - 1));
            string v; vector<int> b;
            while (getline(ss, v, ',')) b.push_back(stoi(v));
            m.buttons.push_back(b);
            p = e + 1;
        }
        int r = solveMachine(m);
        if (r != -1) t += r;
    }
    cout << "Fewest total button presses: " << t << endl;
    return 0;
}

