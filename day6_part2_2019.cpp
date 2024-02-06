
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <unordered_map>

struct Node {
    std::string name;
    std::vector<Node*> children;
    Node* parent;
};

Node* findOrCreateNode(std::string name, std::unordered_map<std::string, Node*>& nodes) {
    if (nodes.find(name) != nodes.end()) {
        return nodes[name];
    }
    Node* node = new Node{name};
    nodes[name] = node;
    return node;
}

std::unordered_map<std::string, Node*> buildOrbitMap(std::ifstream& file) {
    std::unordered_map<std::string, Node*> nodes;
    std::string line;
    while (std::getline(file, line)) {
        size_t pos = line.find(')');
        std::string centerName = line.substr(0, pos);
        std::string orbiterName = line.substr(pos + 1);
        Node* center = findOrCreateNode(centerName, nodes);
        Node* orbiter = findOrCreateNode(orbiterName, nodes);
        center->children.push_back(orbiter);
        orbiter->parent = center;
    }
    return nodes;
}

std::vector<Node*> pathToRoot(Node* node) {
    std::vector<Node*> path;
    while (node != nullptr) {
        path.push_back(node);
        node = node->parent;
    }
    return path;
}

std::pair<int, int> findCommonAncestor(Node* node1, Node* node2) {
    std::vector<Node*> path1 = pathToRoot(node1);
    std::vector<Node*> path2 = pathToRoot(node2);

    int i = path1.size() - 1;
    int j = path2.size() - 1;

    while (i >= 0 && j >= 0 && path1[i] == path2[j]) {
        i--;
        j--;
    }
    return {i + 1, j + 1};
}

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cout << "Error opening file" << std::endl;
        return 1;
    }

    std::unordered_map<std::string, Node*> orbitMap = buildOrbitMap(file);

    auto [transfersYOU, transfersSAN] = findCommonAncestor(orbitMap["YOU"]->parent, orbitMap["SAN"]->parent);
    std::cout << transfersYOU + transfersSAN << std::endl;

    for (auto& nodePair : orbitMap) {
        delete nodePair.second;
    }

    return 0;
}
