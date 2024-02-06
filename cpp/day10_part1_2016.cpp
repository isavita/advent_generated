
#include <iostream>
#include <fstream>
#include <regex>
#include <unordered_map>
#include <vector>
#include <algorithm>

struct Bot {
    std::string lowTo, highTo;
    std::vector<int> chips;
};

int main() {
    std::ifstream file("input.txt");
    std::unordered_map<std::string, Bot*> bots;
    std::regex valueRegex("value (\\d+) goes to (bot \\d+)");
    std::regex givesRegex("(bot \\d+) gives low to (bot \\d+|output \\d+) and high to (bot \\d+|output \\d+)");

    std::string line;
    while (std::getline(file, line)) {
        if (std::regex_match(line, valueRegex)) {
            std::smatch matches;
            std::regex_search(line, matches, valueRegex);
            int value = std::stoi(matches[1]);
            std::string botID = matches[2];

            if (bots.find(botID) == bots.end()) {
                bots[botID] = new Bot();
            }
            bots[botID]->chips.push_back(value);

        } else if (std::regex_match(line, givesRegex)) {
            std::smatch matches;
            std::regex_search(line, matches, givesRegex);
            std::string botID = matches[1];
            std::string lowTo = matches[2];
            std::string highTo = matches[3];

            if (bots.find(botID) == bots.end()) {
                bots[botID] = new Bot();
            }
            bots[botID]->lowTo = lowTo;
            bots[botID]->highTo = highTo;
        }
    }

    while (true) {
        bool action = false;
        for (auto& pair : bots) {
            Bot* b = pair.second;
            if (b->chips.size() == 2) {
                action = true;
                int low = std::min(b->chips[0], b->chips[1]);
                int high = std::max(b->chips[0], b->chips[1]);
                if (low == 17 && high == 61) {
                    std::cout << pair.first << std::endl;
                    return 0;
                }
                b->chips.clear();

                if (bots.find(b->lowTo) == bots.end()) {
                    bots[b->lowTo] = new Bot();
                }
                bots[b->lowTo]->chips.push_back(low);

                if (bots.find(b->highTo) == bots.end()) {
                    bots[b->highTo] = new Bot();
                }
                bots[b->highTo]->chips.push_back(high);
            }
        }
        if (!action) {
            break;
        }
    }

    return 0;
}
