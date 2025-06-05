
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <sstream>
#include <map>
#include <utility>

std::string trim(const std::string& str) {
    size_t first = str.find_first_not_of(" \t\n\r");
    if (std::string::npos == first) {
        return "";
    }
    size_t last = str.find_last_not_of(" \t\n\r");
    return str.substr(first, (last - first + 1));
}

bool isCorrect(const std::vector<int>& update, const std::vector<std::pair<int, int>>& rules) {
    std::map<int, int> pos;
    for (int i = 0; i < update.size(); ++i) {
        pos[update[i]] = i;
    }

    for (const auto& rule : rules) {
        if (pos.count(rule.first) && pos.count(rule.second)) {
            if (pos.at(rule.first) > pos.at(rule.second)) {
                return false;
            }
        }
    }
    return true;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::ifstream inputFile("input.txt");
    if (!inputFile.is_open()) {
        std::cerr << "Error opening input.txt" << std::endl;
        return 1;
    }

    std::vector<std::pair<int, int>> rules;
    std::vector<std::vector<int>> updates;
    std::string line;
    bool parsingRules = true;

    while (std::getline(inputFile, line)) {
        line = trim(line);
        if (line.empty()) {
            continue;
        }

        if (parsingRules) {
            size_t pipePos = line.find('|');
            if (pipePos != std::string::npos) {
                int x = std::stoi(line.substr(0, pipePos));
                int y = std::stoi(line.substr(pipePos + 1));
                rules.push_back({x, y});
            } else {
                parsingRules = false;
            }
        }

        if (!parsingRules) {
            std::vector<int> currentUpdate;
            std::stringstream ss(line);
            std::string segment;
            while (std::getline(ss, segment, ',')) {
                currentUpdate.push_back(std::stoi(segment));
            }
            updates.push_back(currentUpdate);
        }
    }

    inputFile.close();

    long long totalSum = 0;
    for (const auto& u : updates) {
        if (isCorrect(u, rules)) {
            if (!u.empty()) {
                totalSum += u[u.size() / 2];
            }
        }
    }

    std::cout << totalSum << std::endl;

    return 0;
}
