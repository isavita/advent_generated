
#include <iostream>
#include <fstream>
#include <vector>

using namespace std;

int dx[] = {-1, 0, 1, -1, 1, -1, 0, 1};
int dy[] = {-1, -1, -1, 0, 0, 1, 1, 1};

int countVisibleOccupied(const vector<string>& seatingArea, int row, int col) {
    int count = 0;
    for (int i = 0; i < 8; ++i) {
        for (int r = row + dy[i], c = col + dx[i]; r >= 0 && r < seatingArea.size() && c >= 0 && c < seatingArea[0].size(); r += dy[i], c += dx[i]) {
            if (seatingArea[r][c] == 'L') break;
            if (seatingArea[r][c] == '#') {
                count++;
                break;
            }
        }
    }
    return count;
}

int main() {
    ifstream file("input.txt");
    vector<string> seatingArea;
    string line;
    while (getline(file, line)) {
        seatingArea.push_back(line);
    }

    bool stabilized = false;
    while (!stabilized) {
        stabilized = true;
        vector<string> newSeatingArea = seatingArea;
        for (int i = 0; i < seatingArea.size(); ++i) {
            for (int j = 0; j < seatingArea[0].size(); ++j) {
                if (seatingArea[i][j] == 'L' && countVisibleOccupied(seatingArea, i, j) == 0) {
                    newSeatingArea[i][j] = '#';
                    stabilized = false;
                } else if (seatingArea[i][j] == '#' && countVisibleOccupied(seatingArea, i, j) >= 5) {
                    newSeatingArea[i][j] = 'L';
                    stabilized = false;
                }
            }
        }
        seatingArea = newSeatingArea;
    }

    int occupiedSeats = 0;
    for (const string& row : seatingArea) {
        for (char seat : row) {
            if (seat == '#') {
                occupiedSeats++;
            }
        }
    }
    cout << occupiedSeats << endl;

    return 0;
}
