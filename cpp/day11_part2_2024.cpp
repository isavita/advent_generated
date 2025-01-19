
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <map>
#include <algorithm>

using namespace std;

string trimLeadingZeros(string s) {
    int i = 0;
    while (i < s.length() - 1 && s[i] == '0') {
        i++;
    }
    return s.substr(i);
}

pair<string, string> splitStone(string s) {
    int mid = s.length() / 2;
    string left = trimLeadingZeros(s.substr(0, mid));
    string right = trimLeadingZeros(s.substr(mid));
    if (left.empty()) {
        left = "0";
    }
    if (right.empty()) {
        right = "0";
    }
    return make_pair(left, right);
}

string multiplyBy2024(string s) {
    string multiplier = "2024";
    vector<int> result(s.length() + multiplier.length(), 0);
    for (int i = s.length() - 1; i >= 0; i--) {
        int carry = 0;
        for (int j = multiplier.length() - 1; j >= 0; j--) {
            int product = (s[i] - '0') * (multiplier[j] - '0') + result[i + j + 1] + carry;
            result[i + j + 1] = product % 10;
            carry = product / 10;
        }
        result[i] += carry;
    }
    int start = 0;
    while (start < result.size() - 1 && result[start] == 0) {
        start++;
    }
    string res = "";
    for (; start < result.size(); start++) {
        res += to_string(result[start]);
    }
    return res;
}

int main() {
    ifstream file("input.txt");
    if (!file.is_open()) {
        cerr << "Error opening input.txt" << endl;
        return 1;
    }

    string line;
    if (!getline(file, line)) {
        cerr << "Input file is empty" << endl;
        return 1;
    }

    map<string, long long> stonesMap;
    string word;
    for (int i = 0; i < line.length(); ++i)
    {
        if (line[i] == ' ')
        {
            if (!word.empty())
            {
                stonesMap[word]++;
                word = "";
            }
        }
        else
        {
            word += line[i];
        }
    }
    if (!word.empty())
        stonesMap[word]++;

    for (int step = 0; step < 75; step++) {
        map<string, long long> newStonesMap;
        for (auto const& [stone, count] : stonesMap) {
            if (stone == "0") {
                newStonesMap["1"] += count;
            } else if (stone.length() % 2 == 0) {
                pair<string, string> parts = splitStone(stone);
                newStonesMap[parts.first] += count;
                newStonesMap[parts.second] += count;
            } else {
                string newStone = multiplyBy2024(stone);
                newStonesMap[newStone] += count;
            }
        }
        stonesMap = newStonesMap;
    }

    long long totalStones = 0;
    for (auto const& [stone, count] : stonesMap) {
        totalStones += count;
    }

    cout << totalStones << endl;

    return 0;
}
