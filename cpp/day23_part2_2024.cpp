
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <set>
#include <map>
#include <algorithm> // For std::sort, std::set_intersection

class BronKerboschSolver {
private:
    std::map<std::string, int> nameToId;
    std::vector<std::string> idToName;
    std::vector<std::set<int>> adj; // Adjacency list using integer IDs
    int nextId = 0;

    std::vector<int> bestClique;

    int assignId(const std::string& name) {
        if (nameToId.find(name) == nameToId.end()) {
            nameToId[name] = nextId;
            idToName.push_back(name);
            return nextId++;
        }
        return nameToId[name];
    }

    std::set<int> setIntersection(const std::set<int>& s1, const std::set<int>& s2) {
        std::set<int> result;
        std::set_intersection(s1.begin(), s1.end(),
                              s2.begin(), s2.end(),
                              std::inserter(result, result.begin()));
        return result;
    }

    void bronKerbosch(std::vector<int> R, std::set<int> P, std::set<int> X) {
        if (P.empty() && X.empty()) {
            if (R.size() > bestClique.size()) {
                bestClique = R;
            }
            return;
        }

        std::set<int> P_copy = P; // Iterate over a snapshot of P

        for (int v : P_copy) {
            std::vector<int> R_next = R;
            R_next.push_back(v);

            const std::set<int>& neighbors = adj[v];

            std::set<int> P_next = setIntersection(P, neighbors);
            std::set<int> X_next = setIntersection(X, neighbors);

            bronKerbosch(R_next, P_next, X_next);

            P.erase(v); // Remove v from P for subsequent iterations at this level
            X.insert(v); // Add v to X for subsequent iterations at this level
        }
    }

public:
    void readInput(const std::string& filename) {
        std::ifstream file(filename);
        std::string line;

        std::set<std::string> allNodeNames;
        std::vector<std::pair<std::string, std::string>> edgeNames;

        while (std::getline(file, line)) {
            size_t dashPos = line.find('-');
            if (dashPos == std::string::npos) continue;

            std::string a = line.substr(0, dashPos);
            std::string b = line.substr(dashPos + 1);

            allNodeNames.insert(a);
            allNodeNames.insert(b);
            edgeNames.push_back({a, b});
        }
        file.close();

        for (const std::string& nodeName : allNodeNames) {
            assignId(nodeName); // Assign integer IDs to all unique node names
        }
        adj.resize(nextId); // Resize adjacency list based on total unique nodes

        for (const auto& edge : edgeNames) {
            int u = nameToId[edge.first];
            int v = nameToId[edge.second];
            adj[u].insert(v);
            adj[v].insert(u);
        }
    }

    void solve() {
        std::set<int> initialP;
        for (int i = 0; i < nextId; ++i) {
            initialP.insert(i); // All nodes are initially candidates
        }

        bronKerbosch({}, initialP, {});

        std::vector<std::string> resultNames;
        for (int id : bestClique) {
            resultNames.push_back(idToName[id]);
        }

        std::sort(resultNames.begin(), resultNames.end());

        for (size_t i = 0; i < resultNames.size(); ++i) {
            std::cout << resultNames[i] << (i == resultNames.size() - 1 ? "" : ",");
        }
        std::cout << std::endl;
    }
};

int main() {
    BronKerboschSolver solver;
    solver.readInput("input.txt");
    solver.solve();
    return 0;
}
