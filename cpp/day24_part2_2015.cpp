
#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <sstream>
#include <climits>
#include <algorithm>

using namespace std;

bool canSplit(const vector<int>& packages, int firstGroupComb, int targetWeight) {
    vector<int> remainingPackages;
    for (size_t i = 0; i < packages.size(); ++i) {
        if (!(firstGroupComb & (1 << i))) {
            remainingPackages.push_back(packages[i]);
        }
    }

    for (int comb1 = 1; comb1 < (1 << remainingPackages.size()); ++comb1) {
        int group1Weight = 0;
        for (size_t i = 0; i < remainingPackages.size(); ++i) {
            if (comb1 & (1 << i)) {
                group1Weight += remainingPackages[i];
            }
        }
        if (group1Weight == targetWeight) {
            for (int comb2 = 1; comb2 < (1 << remainingPackages.size()); ++comb2) {
                if ((comb1 & comb2) == 0) {
                    int group2Weight = 0;
                    for (size_t i = 0; i < remainingPackages.size(); ++i) {
                        if (comb2 & (1 << i)) {
                            group2Weight += remainingPackages[i];
                        }
                    }
                    if (group2Weight == targetWeight) {
                        return true;
                    }
                }
            }
        }
    }
    return false;
}

int main() {
    ifstream inputFile("input.txt");
    if (!inputFile.is_open()) {
        cerr << "Error opening input.txt" << endl;
        return 1;
    }

    vector<int> packages;
    int totalWeight = 0;
    string line;
    while (getline(inputFile, line)) {
        try {
            packages.push_back(stoi(line));
            totalWeight += stoi(line);
        } catch (const invalid_argument& e) {
            cerr << "Invalid input: " << line << endl;
            return 1;
        }
    }
    inputFile.close();

    int targetWeight = totalWeight / 4;
    long long bestQE = LLONG_MAX;
    int bestLength = INT_MAX;

    for (int comb = 1; comb < (1 << packages.size()); ++comb) {
        int groupWeight = 0;
        long long qe = 1;
        int groupLength = 0;
        for (size_t i = 0; i < packages.size(); ++i) {
            if (comb & (1 << i)) {
                groupWeight += packages[i];
                qe *= packages[i];
                groupLength++;
            }
        }
        if (groupWeight == targetWeight && groupLength <= bestLength) {
            if (groupLength < bestLength || qe < bestQE) {
                if (canSplit(packages, comb, targetWeight)) {
                    bestLength = groupLength;
                    bestQE = qe;
                }
            }
        }
    }

    cout << bestQE << endl;

    return 0;
}
