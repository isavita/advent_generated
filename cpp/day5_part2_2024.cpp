
#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <sstream>
#include <utility>
#include <unordered_map>
#include <unordered_set>

bool is_correct(const std::vector<int>& update, const std::vector<std::pair<int, int>>& rules) {
    std::unordered_map<int, int> pos;
    for (int i = 0; i < update.size(); ++i) {
        pos[update[i]] = i;
    }

    for (const auto& rule : rules) {
        int x = rule.first;
        int y = rule.second;

        auto it_x = pos.find(x);
        auto it_y = pos.find(y);

        if (it_x != pos.end() && it_y != pos.end()) {
            if (it_x->second > it_y->second) {
                return false;
            }
        }
    }
    return true;
}

std::vector<int> correct_order(const std::vector<int>& update, const std::vector<std::pair<int, int>>& rules) {
    std::unordered_set<int> pages;
    for (int p : update) {
        pages.insert(p);
    }

    std::unordered_map<int, std::vector<int>> adj;
    std::unordered_map<int, int> indeg;

    for (int p : update) {
        adj[p] = {};
        indeg[p] = 0;
    }

    for (const auto& rule : rules) {
        int x = rule.first;
        int y = rule.second;

        if (pages.count(x) && pages.count(y)) {
            adj[x].push_back(y);
            indeg[y]++;
        }
    }

    std::vector<int> q;
    for (int p : update) {
        if (indeg[p] == 0) {
            q.push_back(p);
        }
    }

    std::vector<int> out;
    while (!q.empty()) {
        int n = q.back();
        q.pop_back();
        out.push_back(n);

        for (int next : adj[n]) {
            indeg[next]--;
            if (indeg[next] == 0) {
                q.push_back(next);
            }
        }
    }
    return out;
}

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cerr << "Error opening input.txt" << std::endl;
        return 1;
    }

    std::vector<std::pair<int, int>> rules;
    std::vector<std::vector<int>> updates;
    std::string line;
    bool parsing_rules = true;

    while (std::getline(file, line)) {
        if (line.empty()) {
            continue;
        }

        if (parsing_rules) {
            size_t pipe_pos = line.find('|');
            if (pipe_pos != std::string::npos) {
                int x = std::stoi(line.substr(0, pipe_pos));
                int y = std::stoi(line.substr(pipe_pos + 1));
                rules.push_back({x, y});
            } else {
                parsing_rules = false;
                std::vector<int> current_update;
                std::stringstream ss(line);
                std::string segment;
                while (std::getline(ss, segment, ',')) {
                    current_update.push_back(std::stoi(segment));
                }
                updates.push_back(current_update);
            }
        } else {
            std::vector<int> current_update;
            std::stringstream ss(line);
            std::string segment;
            while (std::getline(ss, segment, ',')) {
                current_update.push_back(std::stoi(segment));
            }
            updates.push_back(current_update);
        }
    }
    file.close();

    long long sum_incorrect_middle_elements = 0;
    for (const auto& u : updates) {
        if (!is_correct(u, rules)) {
            std::vector<int> fixed = correct_order(u, rules);
            if (!fixed.empty()) {
                sum_incorrect_middle_elements += fixed[fixed.size() / 2];
            }
        }
    }

    std::cout << sum_incorrect_middle_elements << std::endl;

    return 0;
}
