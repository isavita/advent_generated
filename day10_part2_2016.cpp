#include <iostream>
#include <fstream>
#include <regex>
#include <map>
#include <vector>
#include <algorithm>

using namespace std;

struct bot {
    string lowTo, highTo;
    vector<int> chips;
};

void giveChip(map<string, bot*>& bots, map<string, int>& outputs, string target, int value) {
    if (target.substr(0, 3) == "bot") {
        if (bots.find(target) == bots.end()) {
            bots[target] = new bot();
        }
        bots[target]->chips.push_back(value);
    } else if (target.substr(0, 6) == "output") {
        outputs[target] = value;
    }
}

pair<int, int> minMax(int a, int b) {
    return make_pair(min(a, b), max(a, b));
}

int main() {
    ifstream file("input.txt");
    if (!file.is_open()) {
        cerr << "Error opening file" << endl;
        return 1;
    }

    map<string, bot*> bots;
    map<string, int> outputs;
    regex valueRegex("value (\\d+) goes to (bot \\d+)");
    regex givesRegex("(bot \\d+) gives low to (bot \\d+|output \\d+) and high to (bot \\d+|output \\d+)");

    string line;
    while (getline(file, line)) {
        smatch matches;
        if (regex_search(line, matches, valueRegex)) {
            int value = stoi(matches[1]);
            string botID = matches[2];

            if (bots.find(botID) == bots.end()) {
                bots[botID] = new bot();
            }
            bots[botID]->chips.push_back(value);

        } else if (regex_search(line, matches, givesRegex)) {
            string botID = matches[1];
            string lowTo = matches[2];
            string highTo = matches[3];

            if (bots.find(botID) == bots.end()) {
                bots[botID] = new bot();
            }
            bots[botID]->lowTo = lowTo;
            bots[botID]->highTo = highTo;
        }
    }

    while (true) {
        bool action = false;
        for (auto& b : bots) {
            if (b.second->chips.size() == 2) {
                action = true;
                pair<int, int> minMaxValues = minMax(b.second->chips[0], b.second->chips[1]);
                b.second->chips.clear();

                giveChip(bots, outputs, b.second->lowTo, minMaxValues.first);
                giveChip(bots, outputs, b.second->highTo, minMaxValues.second);
            }
        }
        if (!action) {
            break;
        }
    }

    int result = outputs["output 0"] * outputs["output 1"] * outputs["output 2"];
    cout << result << endl;

    for (auto& b : bots) {
        delete b.second;
    }

    return 0;
}