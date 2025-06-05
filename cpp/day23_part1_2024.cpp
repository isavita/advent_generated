
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <map>
#include <set>
#include <algorithm>
#include <iterator>

void solve() {
    std::map<std::string, std::set<std::string>> graph;
    std::ifstream file("input.txt");
    std::string line;

    while (std::getline(file, line)) {
        size_t dash_pos = line.find('-');
        std::string a = line.substr(0, dash_pos);
        std::string b = line.substr(dash_pos + 1);

        graph[a].insert(b);
        graph[b].insert(a);
    }
    file.close();

    long long count = 0;
    std::vector<std::string> nodes;

    for (const auto& pair : graph) {
        nodes.push_back(pair.first);
    }
    std::sort(nodes.begin(), nodes.end());

    for (size_t i = 0; i < nodes.size(); ++i) {
        const std::string& u = nodes[i];
        
        for (const std::string& v : graph[u]) {
            if (v <= u) continue;

            std::vector<std::string> common_neighbors;
            std::set_intersection(graph[u].begin(), graph[u].end(),
                                  graph[v].begin(), graph[v].end(),
                                  std::back_inserter(common_neighbors));

            for (const std::string& w : common_neighbors) {
                if (w <= v) continue;
                
                bool t_condition = (u[0] == 't') || (v[0] == 't') || (w[0] == 't');

                if (t_condition) {
                    count++;
                }
            }
        }
    }

    std::cout << count << std::endl;
}

int main() {
    solve();
    return 0;
}

