
#include <fstream>
#include <iostream>
#include <string>
#include <vector>
#include <unordered_map>

using namespace std;

unordered_map<string, vector<string>> graph;

int dfs(const string& node) {
    if (node == "out") return 1;
    int total = 0;
    for (const string& neighbor : graph[node]) {
        total += dfs(neighbor);
    }
    return total;
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    ifstream in("input.txt");
    string line;

    while (getline(in, line)) {
        size_t colon = line.find(':');
        string from = line.substr(0, colon);
        string rest = line.substr(colon + 2);
        size_t start = 0;
        while (start < rest.size()) {
            size_t space = rest.find(' ', start);
            if (space == string::npos) space = rest.size();
            string to = rest.substr(start, space - start);
            graph[from].push_back(to);
            start = space + 1;
        }
    }

    cout << dfs("you") << '\n';
    return 0;
}
