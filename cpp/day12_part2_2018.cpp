
#include <iostream>
#include <fstream>
#include <string>
#include <unordered_map>
#include <vector>
#include <algorithm>

using namespace std;

int main() {
    ifstream file("input.txt");
    string initialState;
    unordered_map<string, char> rules;

    string line;
    while (getline(file, line)) {
        if (line.find("initial state") != string::npos) {
            initialState = line.substr(line.find(": ") + 2);
        } else if (line.find("=>") != string::npos) {
            string pattern = line.substr(0, 5);
            char result = line.back();
            rules[pattern] = result;
        }
    }

    unordered_map<int, char> state;
    for (int i = 0; i < initialState.size(); ++i) {
        if (initialState[i] == '#') {
            state[i] = '#';
        }
    }

    string previousPattern;
    long long previousSum = 0;
    long long offset = 0;
    for (long long generation = 0; generation < 50000000000; ++generation) {
        unordered_map<int, char> newState;
        int minPot = state.begin()->first;
        int maxPot = state.begin()->first;
        for(auto const& [key, val] : state){
            minPot = min(minPot, key);
            maxPot = max(maxPot, key);
        }
        
        for (int i = minPot - 2; i <= maxPot + 2; ++i) {
            string pattern;
            for (int j = i - 2; j <= i + 2; ++j) {
                if (state.count(j) > 0 && state[j] == '#') {
                    pattern += "#";
                } else {
                    pattern += ".";
                }
            }
            if (rules.count(pattern) > 0 && rules[pattern] == '#') {
                newState[i] = '#';
            }
        }
        state = newState;

        string currentPattern;
        long long currentSum = 0;
        
        minPot = state.begin()->first;
        maxPot = state.begin()->first;
        for(auto const& [key, val] : state){
            minPot = min(minPot, key);
            maxPot = max(maxPot, key);
        }
        
        for (int i = minPot; i <= maxPot; ++i) {
            if (state.count(i) > 0 && state[i] == '#') {
                currentPattern += "#";
                currentSum += i;
            } else {
                currentPattern += ".";
            }
        }

        if (currentPattern == previousPattern) {
            offset = currentSum - previousSum;
            long long remainingGenerations = 50000000000 - generation - 1;
            long long finalSum = currentSum + offset * remainingGenerations;
            cout << finalSum << endl;
            return 0;
        }
        previousPattern = currentPattern;
        previousSum = currentSum;
    }

    return 0;
}
