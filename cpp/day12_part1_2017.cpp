
#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <unordered_map>

std::unordered_map<int, std::vector<int> > adj;
std::unordered_map<int, bool> visited;

void DFS(int node);

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cout << "File reading error" << std::endl;
        return 1;
    }

    std::string line;
    while (std::getline(file, line)) {
        std::istringstream iss(line);
        std::string fromStr, toStr;
        std::getline(iss, fromStr, ' ');
        int from = std::stoi(fromStr);
        std::getline(iss, toStr, '>');
        std::vector<std::string> toNodes;
        while (std::getline(iss, toStr, ',')) {
            toNodes.push_back(toStr);
        }

        for (const auto& toNode : toNodes) {
            int to = std::stoi(toNode);
            adj[from].push_back(to);
            adj[to].push_back(from);
        }
    }

    DFS(0);

    int count = 0;
    for (const auto& v : visited) {
        if (v.second) {
            count++;
        }
    }

    std::cout << count << std::endl;

    return 0;
}

void DFS(int node) {
    visited[node] = true;
    for (const auto& neighbor : adj[node]) {
        if (!visited[neighbor]) {
            DFS(neighbor);
        }
    }
}
