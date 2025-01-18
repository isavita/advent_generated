
#include <iostream>
#include <fstream>
#include <string>
#include <unordered_map>
#include <vector>
#include <algorithm>

using namespace std;

void parseInput(const string& filename, string& templateStr, unordered_map<string, char>& rules) {
    ifstream file(filename);
    if (!file.is_open()) {
        cerr << "Error opening file: " << filename << endl;
        exit(1);
    }

    getline(file, templateStr);

    string line;
    while (getline(file, line)) {
        if (line.empty()) continue;
        rules[line.substr(0, 2)] = line[6];
    }

    file.close();
}

void applyRules(const string& templateStr, const unordered_map<string, char>& rules, unordered_map<string, long long>& pairCounts, unordered_map<char, long long>& charCounts) {
    unordered_map<string, long long> newPairCounts;

    for (const auto& pair : pairCounts) {
        char insertChar = rules.at(pair.first);
        string newPair1 = pair.first[0] + string(1, insertChar);
        string newPair2 = string(1, insertChar) + pair.first[1];

        newPairCounts[newPair1] += pair.second;
        newPairCounts[newPair2] += pair.second;

        charCounts[insertChar] += pair.second;
    }

    pairCounts = newPairCounts;
}

int main() {
    string templateStr;
    unordered_map<string, char> rules;
    parseInput("input.txt", templateStr, rules);

    unordered_map<string, long long> pairCounts;
    unordered_map<char, long long> charCounts;

    for (size_t i = 0; i < templateStr.size() - 1; ++i) {
        string pair = templateStr.substr(i, 2);
        pairCounts[pair]++;
    }

    for (char c : templateStr) {
        charCounts[c]++;
    }

    for (int step = 0; step < 40; ++step) {
        applyRules(templateStr, rules, pairCounts, charCounts);
    }

    long long maxCount = 0;
    long long minCount = LLONG_MAX;

    for (const auto& count : charCounts) {
        maxCount = max(maxCount, count.second);
        minCount = min(minCount, count.second);
    }

    cout << "Result: " << maxCount - minCount << endl;

    return 0;
}
