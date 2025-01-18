
#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <algorithm>

using namespace std;

string trimLeadingZeros(string s) {
    while (s.length() > 1 && s[0] == '0') {
        s.erase(0, 1);
    }
    return s;
}

bool evenDigits(const string& s) {
    return s.length() % 2 == 0;
}

int main() {
    ifstream inputFile("input.txt");
    string line;
    getline(inputFile, line);

    vector<string> stones;
    string currentStone;
    for (char c : line) {
        if (c == ' ') {
            if (!currentStone.empty()) {
                stones.push_back(currentStone);
                currentStone = "";
            }
        } else {
            currentStone += c;
        }
    }
    if (!currentStone.empty()) {
        stones.push_back(currentStone);
    }

    for (int i = 0; i < 25; ++i) {
        vector<string> next;
        next.reserve(stones.size() * 2);
        for (const string& s : stones) {
            if (s == "0") {
                next.push_back("1");
            } else if (evenDigits(s)) {
                int mid = s.length() / 2;
                string left = trimLeadingZeros(s.substr(0, mid));
                string right = trimLeadingZeros(s.substr(mid));
                if (left.empty()) left = "0";
                if (right.empty()) right = "0";
                next.push_back(left);
                next.push_back(right);
            } else {
                next.push_back(to_string(stoll(s) * 2024));
            }
        }
        stones = next;
    }

    cout << stones.size() << endl;

    return 0;
}
