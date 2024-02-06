
#include <iostream>
#include <fstream>
#include <unordered_map>
#include <vector>
#include <string>

struct Cave {
    std::unordered_map<std::string, bool> connections;
};

Cave* NewCave() {
    return new Cave;
}

void ConnectTo(Cave* c, std::string name) {
    c->connections[name] = true;
}

void DisconnectFrom(Cave* c, std::string name) {
    c->connections.erase(name);
}

void dfs(std::string current, std::unordered_map<std::string, bool> visited, std::unordered_map<std::string, Cave*>& caves, int& count) {
    if (current == "end") {
        count++;
        return;
    }

    for (const auto& next : caves[current]->connections) {
        if (visited[next.first] && std::tolower(next.first[0]) == next.first[0]) {
            continue;
        }

        std::unordered_map<std::string, bool> visitedCopy = visited;
        visitedCopy[next.first] = true;
        dfs(next.first, visitedCopy, caves, count);
    }
}

int main() {
    std::ifstream file("input.txt");
    std::unordered_map<std::string, Cave*> caves;

    std::string line;
    while (std::getline(file, line)) {
        std::vector<std::string> paths;
        size_t pos = 0;
        while ((pos = line.find("-")) != std::string::npos) {
            paths.push_back(line.substr(0, pos));
            line.erase(0, pos + 1);
        }
        paths.push_back(line);

        std::string from = paths[0];
        std::string to = paths[1];

        if (caves.find(from) == caves.end()) {
            caves[from] = NewCave();
        }

        if (caves.find(to) == caves.end()) {
            caves[to] = NewCave();
        }

        ConnectTo(caves[from], to);
        ConnectTo(caves[to], from);
    }

    int count = 0;
    std::unordered_map<std::string, bool> visited = {{"start", true}};
    dfs("start", visited, caves, count);

    std::cout << count << std::endl;

    for (auto& cave : caves) {
        delete cave.second;
    }

    return 0;
}
