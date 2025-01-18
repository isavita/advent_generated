
#include <iostream>
#include <fstream>
#include <vector>
#include <string>

using namespace std;

void tiltNorth(vector<string>& platform) {
    int rows = platform.size();
    int cols = platform[0].size();

    for (int col = 0; col < cols; ++col) {
        int emptyRow = -1;
        for (int row = 0; row < rows; ++row) {
            if (platform[row][col] == 'O') {
                if (emptyRow != -1) {
                    platform[emptyRow][col] = 'O';
                    platform[row][col] = '.';
                    ++emptyRow;
                }
            } else if (platform[row][col] == '.') {
                if (emptyRow == -1) {
                    emptyRow = row;
                }
            } else {
                emptyRow = -1;
            }
        }
    }
}

int calculateLoad(const vector<string>& platform) {
    int rows = platform.size();
    int cols = platform[0].size();
    int totalLoad = 0;

    for (int row = 0; row < rows; ++row) {
        for (int col = 0; col < cols; ++col) {
            if (platform[row][col] == 'O') {
                totalLoad += rows - row;
            }
        }
    }

    return totalLoad;
}

int main() {
    ifstream inputFile("input.txt");
    if (!inputFile) {
        cerr << "Error opening input file." << endl;
        return 1;
    }

    vector<string> platform;
    string line;
    while (getline(inputFile, line)) {
        platform.push_back(line);
    }
    inputFile.close();

    tiltNorth(platform);
    int totalLoad = calculateLoad(platform);

    cout << "Total load on the north support beams: " << totalLoad << endl;

    return 0;
}
