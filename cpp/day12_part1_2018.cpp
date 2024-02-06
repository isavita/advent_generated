
#include <iostream>
#include <fstream>
#include <string>
#include <map>

int main() {
    std::ifstream file("input.txt");
    std::string initialState;
    std::map<std::string, char> rules;

    std::string line;
    while (std::getline(file, line)) {
        if (line.find("initial state") != std::string::npos) {
            initialState = line.substr(line.find(": ") + 2);
        } else if (line.find("=>") != std::string::npos) {
            std::string pattern = line.substr(0, line.find(" => "));
            char result = line.back();
            rules[pattern] = result;
        }
    }

    std::map<int, char> state;
    for (int i = 0; i < initialState.length(); i++) {
        if (initialState[i] == '#') {
            state[i] = '#';
        }
    }

    for (int generation = 0; generation < 20; generation++) {
        std::map<int, char> newState;
        int minPot, maxPot;
        for (const auto& kv : state) {
            if (kv.second == '#') {
                minPot = kv.first;
                break;
            }
        }
        for (auto it = state.rbegin(); it != state.rend(); ++it) {
            if (it->second == '#') {
                maxPot = it->first;
                break;
            }
        }
        for (int i = minPot - 2; i <= maxPot + 2; i++) {
            std::string pattern = "";
            for (int j = i - 2; j <= i + 2; j++) {
                if (state.find(j) != state.end() && state[j] == '#') {
                    pattern += "#";
                } else {
                    pattern += ".";
                }
            }
            if (rules[pattern] == '#') {
                newState[i] = '#';
            }
        }
        state = newState;
    }

    int sum = 0;
    for (const auto& kv : state) {
        sum += kv.first;
    }

    std::cout << sum << std::endl;

    return 0;
}
