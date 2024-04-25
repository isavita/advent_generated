#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <unordered_map>
#include <cmath>

using namespace std;

struct Position {
    int x, y;
};

bool operator==(const Position& a, const Position& b) {
    return a.x == b.x && a.y == b.y;
}

struct PositionHash {
    size_t operator()(const Position& p) const {
        return p.x * 31 + p.y;
    }
};

int abs(int x) {
    return x < 0 ? -x : x;
}

int firstRevisitedDistance(vector<string> instructions) {
    Position pos = {0, 0};
    unordered_map<Position, bool, PositionHash> visited;
    visited[pos] = true;
    Position directions[4] = {{0, 1}, {1, 0}, {0, -1}, {-1, 0}};
    int dirIndex = 0; // Start facing North

    for (const string& instruction : instructions) {
        char turn = instruction[0];
        int blocks = stoi(instruction.substr(1));

        if (turn == 'R') {
            dirIndex = (dirIndex + 1) % 4;
        } else {
            dirIndex = (dirIndex - 1 + 4) % 4;
        }

        for (int i = 0; i < blocks; i++) {
            pos.x += directions[dirIndex].x;
            pos.y += directions[dirIndex].y;

            if (visited.count(pos)) {
                return abs(pos.x) + abs(pos.y);
            }
            visited[pos] = true;
        }
    }

    return -1; // No location visited twice
}

int main() {
    ifstream file("input.txt");
    if (!file) {
        cerr << "Unable to open file.\n";
        return 1;
    }

    string line;
    if (!getline(file, line)) {
        cerr << "Invalid input format.\n";
        return 1;
    }

    stringstream ss(line);
    vector<string> instructions;
    string instruction;
    while (ss >> instruction) {
        instructions.push_back(instruction);
    }

    if (instructions.empty()) {
        cerr << "Invalid input format.\n";
        return 1;
    }

    cout << firstRevisitedDistance(instructions) << endl;

    return 0;
}