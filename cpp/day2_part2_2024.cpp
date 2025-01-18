
#include <iostream>
#include <fstream>
#include <vector>
#include <sstream>
#include <cmath>
#include <algorithm>

using namespace std;

bool isSafeReport(const vector<int>& levels) {
    if (levels.size() < 2) return false;
    int firstDiff = levels[1] - levels[0];
    if (firstDiff == 0) return false;
    bool isIncreasing = firstDiff > 0;
    for (size_t i = 0; i < levels.size() - 1; ++i) {
        int diff = levels[i + 1] - levels[i];
        if (diff == 0) return false;
        if ((isIncreasing && diff <= 0) || (!isIncreasing && diff >= 0)) return false;
        int absDiff = abs(diff);
        if (absDiff < 1 || absDiff > 3) return false;
    }
    return true;
}

bool isSafeWithOneRemoval(const vector<int>& levels) {
    for (size_t i = 0; i < levels.size(); ++i) {
        vector<int> modifiedLevels;
        for(size_t j = 0; j < levels.size(); ++j){
            if(i != j){
                modifiedLevels.push_back(levels[j]);
            }
        }
        if (isSafeReport(modifiedLevels)) return true;
    }
    return false;
}

int main() {
    ifstream file("input.txt");
    if (!file.is_open()) {
        cerr << "Failed to open input file" << endl;
        return 1;
    }

    string line;
    int safeReportCount = 0;
    while (getline(file, line)) {
        vector<int> levels;
        stringstream ss(line);
        int level;
        while (ss >> level) {
            levels.push_back(level);
        }
        if (isSafeReport(levels) || isSafeWithOneRemoval(levels)) {
            safeReportCount++;
        }
    }

    cout << safeReportCount << endl;
    return 0;
}
