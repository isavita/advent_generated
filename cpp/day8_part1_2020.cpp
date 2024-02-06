#include <iostream>
#include <fstream>
#include <vector>
#include <unordered_map>
#include <sstream>

using namespace std;

pair<int, bool> executeBootCode(vector<string>& instructions) {
    int accumulator = 0;
    unordered_map<int, bool> visited;
    int currentInstruction = 0;

    while (currentInstruction < instructions.size()) {
        if (visited[currentInstruction]) {
            return make_pair(accumulator, true);
        }

        visited[currentInstruction] = true;
        stringstream ss(instructions[currentInstruction]);
        string op;
        ss >> op;
        int arg;
        ss >> arg;

        if (op == "acc") {
            accumulator += arg;
            currentInstruction++;
        } else if (op == "jmp") {
            currentInstruction += arg;
        } else {
            currentInstruction++;
        }
    }

    return make_pair(accumulator, false);
}

int main() {
    ifstream file("input.txt");
    if (!file.is_open()) {
        cout << "Error opening file" << endl;
        return 1;
    }

    vector<string> instructions;
    string line;
    while (getline(file, line)) {
        instructions.push_back(line);
    }

    file.close();

    pair<int, bool> result = executeBootCode(instructions);
    cout << result.first << endl;

    return 0;
}