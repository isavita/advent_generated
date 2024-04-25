#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <unordered_map>

using namespace std;

struct Row {
    string springs;
    vector<int> group;
};

vector<Row> parseInput(vector<string> input) {
    vector<Row> rows;
    for (const string& line : input) {
        stringstream ss(line);
        string springs;
        getline(ss, springs, ' ');
        string numbersLine;
        getline(ss, numbersLine);
        vector<int> group;
        stringstream ss2(numbersLine);
        string num;
        while (getline(ss2, num, ',')) {
            group.push_back(stoi(num));
        }
        Row row = {springs, group};
        rows.push_back(row);
    }
    return rows;
}

int countArrangementsRecursive(const Row& row, int iSprings, int iGroup, int iContiguousDamaged, unordered_map<int, int>& cache) {
    int cacheKey = iSprings * 1000000 + iGroup * 1000 + iContiguousDamaged;
    if (cache.find(cacheKey) != cache.end()) {
        return cache[cacheKey];
    }

    if (iSprings == row.springs.size()) {
        if (iGroup == row.group.size() && iContiguousDamaged == 0) {
            return 1;
        } else if (iGroup == row.group.size() - 1 && iContiguousDamaged == row.group[iGroup]) {
            return 1;
        }
        return 0;
    }

    int res = 0;
    char c = row.springs[iSprings];
    if (c == '.' || c == '?') {
        if (iContiguousDamaged == 0) {
            res += countArrangementsRecursive(row, iSprings + 1, iGroup, iContiguousDamaged, cache);
        } else if (iContiguousDamaged == row.group[iGroup]) {
            res += countArrangementsRecursive(row, iSprings + 1, iGroup + 1, 0, cache);
        }
    }
    if (c == '#' || c == '?') {
        if (iGroup < row.group.size() && iContiguousDamaged < row.group[iGroup]) {
            res += countArrangementsRecursive(row, iSprings + 1, iGroup, iContiguousDamaged + 1, cache);
        }
    }

    cache[cacheKey] = res;
    return res;
}

int countArrangements(const Row& row) {
    unordered_map<int, int> cache;
    return countArrangementsRecursive(row, 0, 0, 0, cache);
}

Row unfoldRow(const Row& row, int unfoldingFactor) {
    Row newRow = row;
    for (int i = 1; i < unfoldingFactor; i++) {
        newRow.springs += "?" + row.springs;
        newRow.group.insert(newRow.group.end(), row.group.begin(), row.group.end());
    }
    return newRow;
}

int solve(vector<string> input) {
    vector<Row> rows = parseInput(input);

    int res = 0;
    for (const Row& row : rows) {
        res += countArrangements(row);
    }

    return res;
}

vector<string> readFile(const string& fileName) {
    ifstream file(fileName);
    vector<string> input;
    string line;
    while (getline(file, line)) {
        input.push_back(line);
    }
    return input;
}

int main() {
    vector<string> input = readFile("input.txt");
    cout << solve(input) << endl;
    return 0;
}