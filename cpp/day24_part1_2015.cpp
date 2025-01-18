
#include <iostream>
#include <fstream>
#include <vector>
#include <climits>
#include <numeric>
#include <algorithm>

using namespace std;

int main() {
    ifstream inputFile("input.txt");
    if (!inputFile.is_open()) {
        cerr << "Error opening input file" << endl;
        return 1;
    }

    vector<int> packages;
    int weight;
    int totalWeight = 0;
    while (inputFile >> weight) {
        packages.push_back(weight);
        totalWeight += weight;
    }
    inputFile.close();

    int targetWeight = totalWeight / 3;
    long long bestQE = LLONG_MAX;
    int bestLength = INT_MAX;

    for (int comb = 1; comb < (1 << packages.size()); ++comb) {
        int groupWeight = 0;
        long long qe = 1;
        int groupLength = 0;
        for (int i = 0; i < packages.size(); ++i) {
            if ((comb & (1 << i)) != 0) {
                groupWeight += packages[i];
                qe *= packages[i];
                groupLength++;
            }
        }
        if (groupWeight == targetWeight && groupLength <= bestLength) {
            if (groupLength < bestLength || qe < bestQE) {
                bestLength = groupLength;
                bestQE = qe;
            }
        }
    }

    cout << bestQE << endl;

    return 0;
}
