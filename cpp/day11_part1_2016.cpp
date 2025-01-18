
#include <iostream>
#include <fstream>
#include <vector>
#include <queue>
#include <map>
#include <set>
#include <algorithm>

using namespace std;

struct State {
    int elevatorFloor;
    vector<int> itemFloors;
    int steps;

    bool operator==(const State& other) const {
        return elevatorFloor == other.elevatorFloor && itemFloors == other.itemFloors;
    }
};

namespace std {
    template <>
    struct hash<State> {
        size_t operator()(const State& s) const {
            size_t h = hash<int>()(s.elevatorFloor);
            for (int floor : s.itemFloors) {
                h ^= hash<int>()(floor) + 0x9e3779b9 + (h << 6) + (h >> 2);
            }
            return h;
        }
    };
}

bool isValid(const State& state, int numItems) {
    for (int floor = 0; floor < 4; ++floor) {
        vector<int> generators;
        vector<int> chips;
        for (int i = 0; i < numItems; ++i) {
            if (state.itemFloors[i] == floor) {
                if (i % 2 == 0) {
                    generators.push_back(i);
                } else {
                    chips.push_back(i);
                }
            }
        }

        if (chips.empty()) continue;

        for (int chip : chips) {
            bool hasMatchingGenerator = false;
            for (int generator : generators) {
                if (chip == generator + 1) {
                    hasMatchingGenerator = true;
                    break;
                }
            }
            if (!hasMatchingGenerator && !generators.empty()) {
                return false;
            }
        }
    }
    return true;
}

int solve(State initialState, int numItems) {
    queue<State> q;
    q.push(initialState);
    set<State> visited;
    visited.insert(initialState);

    while (!q.empty()) {
        State current = q.front();
        q.pop();

        bool allOnFourth = true;
        for (int floor : current.itemFloors) {
            if (floor != 3) {
                allOnFourth = false;
                break;
            }
        }
        if (allOnFourth) {
            return current.steps;
        }

        for (int nextFloor = max(0, current.elevatorFloor - 1); nextFloor <= min(3, current.elevatorFloor + 1); ++nextFloor) {
            if (nextFloor == current.elevatorFloor) continue;

            for (int i = 0; i < (1 << numItems); ++i) {
                vector<int> itemsToMove;
                int count = 0;
                for (int j = 0; j < numItems; ++j) {
                    if ((i >> j) & 1) {
                        if (current.itemFloors[j] == current.elevatorFloor) {
                            itemsToMove.push_back(j);
                            count++;
                        } else {
                            count = 3;
                            break;
                        }
                    }
                }
                if (count == 0 || count > 2) continue;

                State nextState = current;
                nextState.elevatorFloor = nextFloor;
                nextState.steps++;
                for (int item : itemsToMove) {
                    nextState.itemFloors[item] = nextFloor;
                }

                if (isValid(nextState, numItems) && visited.find(nextState) == visited.end()) {
                    q.push(nextState);
                    visited.insert(nextState);
                }
            }
        }
    }
    return -1;
}

int main() {
    ifstream inputFile("input.txt");
    if (!inputFile.is_open()) {
        cerr << "Error opening input.txt" << endl;
        return 1;
    }

    string line;
    vector<vector<string>> floors(4);
    int floorNum = 0;
    while (getline(inputFile, line)) {
        size_t pos = line.find("contains ");
        if (pos != string::npos) {
            string items = line.substr(pos + 9);
            size_t commaPos = items.find(", and ");
            while (commaPos != string::npos) {
                string item = items.substr(0, commaPos);
                floors[floorNum].push_back(item);
                items = items.substr(commaPos + 6);
                commaPos = items.find(", and ");
            }
            size_t andPos = items.find(" and ");
            if (andPos != string::npos) {
                string item1 = items.substr(0, andPos);
                string item2 = items.substr(andPos + 5);
                floors[floorNum].push_back(item1);
                floors[floorNum].push_back(item2);
            } else {
                floors[floorNum].push_back(items);
            }
        }
        floorNum++;
    }
    inputFile.close();

    map<string, int> itemMap;
    vector<int> itemFloors;
    int itemIndex = 0;
    for (int i = 0; i < 4; ++i) {
        for (const string& item : floors[i]) {
            string itemName;
            if (item.find("generator") != string::npos) {
                itemName = item.substr(0, item.find(" generator"));
            } else if (item.find("microchip") != string::npos) {
                itemName = item.substr(0, item.find("-compatible microchip"));
            }
            if (itemMap.find(itemName) == itemMap.end()) {
                itemMap[itemName] = itemIndex;
                itemIndex += 2;
            }
            if (item.find("generator") != string::npos) {
                itemFloors.push_back(i);
            } else if (item.find("microchip") != string::npos) {
                itemFloors.push_back(i);
            }
        }
    }

    int numItems = itemFloors.size();
    State initialState = {0, itemFloors, 0};
    int result = solve(initialState, numItems);
    cout << result << endl;

    return 0;
}
