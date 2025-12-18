#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <unordered_map>
#include <sstream>

using namespace std;

long long dfs_path(int cur, int target, const vector<vector<int>>& adj, vector<long long>& memo) {
    if (cur == target) return 1;
    long long &res = memo[cur];
    if (res != -1) return res;
    long long sum = 0;
    for (int v : adj[cur]) sum += dfs_path(v, target, adj, memo);
    res = sum;
    return sum;
}

long long count_paths(int s, int t, const vector<vector<int>>& adj) {
    vector<long long> memo(adj.size(), -1);
    return dfs_path(s, t, adj, memo);
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    unordered_map<string,int> id;
    vector<vector<int>> adj;

    auto get_id = [&](const string& s) -> int {
        auto it = id.find(s);
        if (it != id.end()) return it->second;
        int idx = (int)id.size();
        id[s] = idx;
        adj.push_back(vector<int>());
        return idx;
    };

    auto trim = [](const string& str) -> string {
        size_t a = str.find_first_not_of(" \t\r\n");
        if (a == string::npos) return "";
        size_t b = str.find_last_not_of(" \t\r\n");
        return str.substr(a, b - a + 1);
    };

    ifstream fin("input.txt");
    if (!fin) return 1;
    string line;
    while (getline(fin, line)) {
        auto pos = line.find(':');
        if (pos == string::npos) continue;
        string src = trim(line.substr(0, pos));
        if (src.empty()) continue;
        string dst = trim(line.substr(pos + 1));
        int u = get_id(src);
        string token;
        stringstream ss(dst);
        while (ss >> token) {
            int v = get_id(token);
            adj[u].push_back(v);
        }
    }
    fin.close();

    int svr = get_id("svr");
    int dac = get_id("dac");
    int fft = get_id("fft");
    int out = get_id("out");

    long long s1 = count_paths(svr, dac, adj) * count_paths(dac, fft, adj) * count_paths(fft, out, adj);
    long long s2 = count_paths(svr, fft, adj) * count_paths(fft, dac, adj) * count_paths(dac, out, adj);

    cout << "Paths (svr->dac->fft->out): " << s1 << "\n";
    cout << "Paths (svr->fft->dac->out): " << s2 << "\n";
    cout << "Total paths visiting both: " << (s1 + s2) << "\n";

    return 0;
}