
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <algorithm>

using namespace std;

int main() {
    ifstream inputFile("input.txt");
    if (!inputFile.is_open()) {
        cerr << "Error opening input.txt" << endl;
        return 1;
    }

    string line;
    getline(inputFile, line);
    inputFile.close();

    vector<char> programs;
    for (char c = 'a'; c <= 'p'; ++c) {
        programs.push_back(c);
    }

    string move;
    size_t pos = 0;
    while (pos < line.size()) {
        size_t commaPos = line.find(',', pos);
        if (commaPos == string::npos) {
            move = line.substr(pos);
            pos = line.size();
        } else {
            move = line.substr(pos, commaPos - pos);
            pos = commaPos + 1;
        }

        char type = move[0];
        if (type == 's') {
            int spinSize = stoi(move.substr(1));
            rotate(programs.begin(), programs.end() - spinSize, programs.end());
        } else if (type == 'x') {
            size_t slashPos = move.find('/');
            int posA = stoi(move.substr(1, slashPos - 1));
            int posB = stoi(move.substr(slashPos + 1));
            swap(programs[posA], programs[posB]);
        } else if (type == 'p') {
            size_t slashPos = move.find('/');
            char progA = move[1];
            char progB = move[slashPos + 1];
            int posA = find(programs.begin(), programs.end(), progA) - programs.begin();
            int posB = find(programs.begin(), programs.end(), progB) - programs.begin();
            swap(programs[posA], programs[posB]);
        }
    }

    for (char program : programs) {
        cout << program;
    }
    cout << endl;

    return 0;
}
