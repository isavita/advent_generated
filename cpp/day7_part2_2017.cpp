
#include <iostream>
#include <fstream>
#include <vector>
#include <map>
#include <string>
#include <regex>

using namespace std;

struct Program {
    int weight;
    vector<string> holds;
};

pair<int, bool> dfs(string name, map<string, Program>& programs) {
    Program program = programs[name];
    int totalWeight = program.weight;

    map<int, int> weights;
    for (string child : program.holds) {
        pair<int, bool> result = dfs(child, programs);
        int weight = result.first;
        bool balanced = result.second;
        if (!balanced) {
            return make_pair(0, false);
        }
        totalWeight += weight;
        weights[weight]++;
    }

    for (auto it1 = weights.begin(); it1 != weights.end(); ++it1) {
        for (auto it2 = weights.begin(); it2 != weights.end(); ++it2) {
            if (it1->first != it2->first && it1->second < it2->second) {
                string unbalancedProgram = "";
                for (string child : program.holds) {
                    pair<int, bool> result = dfs(child, programs);
                    if (result.first == it1->first) {
                        unbalancedProgram = child;
                        break;
                    }
                }
                cout << programs[unbalancedProgram].weight + (it2->first - it1->first) << endl;
                return make_pair(0, false);
            }
        }
    }

    return make_pair(totalWeight, true);
}

int main() {
    ifstream inputFile("input.txt");
    if (!inputFile.is_open()) {
        cout << "File reading error" << endl;
        return 1;
    }

    vector<string> lines;
    string line;
    while (getline(inputFile, line)) {
        lines.push_back(line);
    }

    map<string, Program> programs;

    regex re("[a-z]+|\\d+");
    for (string line : lines) {
        auto words_begin = sregex_iterator(line.begin(), line.end(), re);
        auto words_end = sregex_iterator();

        string name = (*words_begin++).str();
        int weight = stoi((*words_begin++).str());

        Program program = {weight};
        while (words_begin != words_end) {
            program.holds.push_back((*words_begin++).str());
        }

        programs[name] = program;
    }

    string root = "dtacyn"; // Replace this with the root found in Part One

    dfs(root, programs);

    return 0;
}
