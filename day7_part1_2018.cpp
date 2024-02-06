
#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <unordered_map>
#include <algorithm>

std::unordered_map<char, std::vector<char> > parseInput(const std::string& filename);
std::string topologicalSort(std::unordered_map<char, std::vector<char> > deps, std::unordered_map<char, bool> allSteps);
bool contains(const std::vector<char>& vec, char val);
std::vector<char> remove(std::vector<char> vec, char val);

int main() {
    std::unordered_map<char, std::vector<char> > deps = parseInput("input.txt");
    std::unordered_map<char, bool> allSteps;
    for (const auto& dep : deps) {
        allSteps[dep.first] = true;
        for (char step : dep.second) {
            allSteps[step] = true;
        }
    }
    std::string order = topologicalSort(deps, allSteps);
    std::cout << order << std::endl;
    return 0;
}

std::unordered_map<char, std::vector<char> > parseInput(const std::string& filename) {
    std::unordered_map<char, std::vector<char> > deps;
    std::ifstream file(filename);
    if (!file.is_open()) {
        throw std::runtime_error("Failed to open file");
    }

    std::string line;
    while (std::getline(file, line)) {
        char a, b;
        std::sscanf(line.c_str(), "Step %c must be finished before step %c can begin.", &a, &b);
        deps[b].push_back(a);
    }

    file.close();
    return deps;
}

std::string topologicalSort(std::unordered_map<char, std::vector<char> > deps, std::unordered_map<char, bool> allSteps) {
    std::string order;
    std::vector<char> available;

    for (const auto& step : allSteps) {
        if (deps[step.first].empty()) {
            available.push_back(step.first);
        }
    }
    std::sort(available.begin(), available.end());

    while (!available.empty()) {
        char next = available[0];
        available.erase(available.begin());
        order.push_back(next);

        for (const auto& step : allSteps) {
            if (contains(deps[step.first], next)) {
                deps[step.first] = remove(deps[step.first], next);
                if (deps[step.first].empty()) {
                    available.push_back(step.first);
                }
            }
        }
        std::sort(available.begin(), available.end());
    }
    return order;
}

bool contains(const std::vector<char>& vec, char val) {
    return std::find(vec.begin(), vec.end(), val) != vec.end();
}

std::vector<char> remove(std::vector<char> vec, char val) {
    vec.erase(std::remove(vec.begin(), vec.end(), val), vec.end());
    return vec;
}
