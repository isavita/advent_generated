#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <unordered_map>

int countCanContain(std::string target, std::unordered_map<std::string, std::vector<std::string> > contains) {
    std::unordered_map<std::string, bool> seen;
    std::function<void(std::string)> dfs = [&](std::string bag) {
        for (auto outer : contains[bag]) {
            if (!seen[outer]) {
                seen[outer] = true;
                dfs(outer);
            }
        }
    };
    dfs(target);
    return seen.size();
}

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cout << "Error opening file\n";
        return 1;
    }

    std::unordered_map<std::string, std::vector<std::string> > contains;
    std::string line;
    while (std::getline(file, line)) {
        size_t pos = line.find(" bags contain ");
        std::string container = line.substr(0, pos);
        if (line.substr(pos + 14) == "no other bags.") {
            continue;
        }
        std::string remaining = line.substr(pos + 14);
        size_t pos1 = remaining.find(", ");
        while (pos1 != std::string::npos) {
            std::string bag = remaining.substr(0, pos1);
            std::string bagName = bag.substr(bag.find(" ") + 1, bag.find_last_of(" ") - bag.find(" ") - 1);
            contains[bagName].push_back(container);
            remaining = remaining.substr(pos1 + 2);
            pos1 = remaining.find(", ");
        }
        std::string bag = remaining.substr(0, remaining.size() - 1);
        std::string bagName = bag.substr(bag.find(" ") + 1, bag.find_last_of(" ") - bag.find(" ") - 1);
        contains[bagName].push_back(container);
    }

    int count = countCanContain("shiny gold", contains);
    std::cout << count << std::endl;

    return 0;
}