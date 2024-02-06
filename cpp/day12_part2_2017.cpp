#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <unordered_map>

void DFS(int node, std::unordered_map<int, std::vector<int>>& adj, std::unordered_map<int, bool>& visited);

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cout << "File reading error" << std::endl;
        return 1;
    }

    std::unordered_map<int, std::vector<int>> adj;
    std::string line;
    while (std::getline(file, line)) {
        std::stringstream ss(line);
        std::string fromStr, toStr;
        std::getline(ss, fromStr, ' ');
        int from = std::stoi(fromStr);
        std::getline(ss, toStr, '>');
        std::vector<int> toNodes;
        while (std::getline(ss, toStr, ',')) {
            int to = std::stoi(toStr);
            adj[from].push_back(to);
            adj[to].push_back(from);
        }
    }

    std::unordered_map<int, bool> visited;
    int groups = 0;
    for (auto& node : adj) {
        if (!visited[node.first]) {
            DFS(node.first, adj, visited);
            groups++;
        }
    }

    std::cout << groups << std::endl;

    return 0;
}

void DFS(int node, std::unordered_map<int, std::vector<int>>& adj, std::unordered_map<int, bool>& visited) {
    visited[node] = true;
    for (int neighbor : adj[node]) {
        if (!visited[neighbor]) {
            DFS(neighbor, adj, visited);
        }
    }
}