#include <iostream>
#include <fstream>
#include <vector>
#include <regex>

struct node {
    int used;
    int avail;
};

std::vector<node> readNodes(std::string filename);
int countViablePairs(std::vector<node> nodes);

int main() {
    std::vector<node> nodes = readNodes("input.txt");
    int viablePairs = countViablePairs(nodes);
    std::cout << viablePairs << std::endl;
}

std::vector<node> readNodes(std::string filename) {
    std::ifstream file(filename);
    if (!file.is_open()) {
        throw std::runtime_error("Error opening file");
    }

    std::vector<node> nodes;
    std::string line;
    std::regex nodeRegex("node-x\\d+-y\\d+\\s+\\d+T\\s+(\\d+)T\\s+(\\d+)T\\s+\\d+%");

    while (std::getline(file, line)) {
        std::smatch matches;
        if (std::regex_search(line, matches, nodeRegex)) {
            int used = std::stoi(matches[1]);
            int avail = std::stoi(matches[2]);
            nodes.push_back({used, avail});
        }
    }

    return nodes;
}

int countViablePairs(std::vector<node> nodes) {
    int count = 0;
    for (size_t i = 0; i < nodes.size(); i++) {
        for (size_t j = 0; j < nodes.size(); j++) {
            if (i != j && nodes[i].used > 0 && nodes[i].used <= nodes[j].avail) {
                count++;
            }
        }
    }
    return count;
}