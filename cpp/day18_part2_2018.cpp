
#include <iostream>
#include <fstream>
#include <vector>
#include <map>

using namespace std;

const char Open = '.';
const char Trees = '|';
const char Lumberyard = '#';
const int Size = 50;

int countAdjacent(const vector<string>& grid, int i, int j, char acreType) {
    int count = 0;
    for (int x = -1; x <= 1; x++) {
        for (int y = -1; y <= 1; y++) {
            if (x == 0 && y == 0) continue;
            if (i + x >= 0 && i + x < Size && j + y >= 0 && j + y < Size && grid[i + x][j + y] == acreType) {
                count++;
            }
        }
    }
    return count;
}

char nextAcreState(const vector<string>& grid, int i, int j) {
    switch (grid[i][j]) {
        case Open:
            if (countAdjacent(grid, i, j, Trees) >= 3) return Trees;
            break;
        case Trees:
            if (countAdjacent(grid, i, j, Lumberyard) >= 3) return Lumberyard;
            break;
        case Lumberyard:
            if (countAdjacent(grid, i, j, Lumberyard) >= 1 && countAdjacent(grid, i, j, Trees) >= 1) return Lumberyard;
            return Open;
    }
    return grid[i][j];
}

int main() {
    ifstream file("input.txt");
    vector<string> grid(Size);
    for (int i = 0; i < Size; i++) {
        file >> grid[i];
    }

    map<long long, int> seenStates;
    int cycleStart, cycleLength;
    for (int minute = 0; ; minute++) {
        long long state = 0;
        for(int i = 0; i < Size; i++){
            for(int j = 0; j < Size; j++){
                state = state * 3;
                if(grid[i][j] == Trees) state += 1;
                else if (grid[i][j] == Lumberyard) state += 2;
            }
        }

        if (seenStates.count(state)) {
            cycleStart = seenStates[state];
            cycleLength = minute - seenStates[state];
            break;
        }
        seenStates[state] = minute;

        vector<string> newGrid = grid;
        for (int i = 0; i < Size; i++) {
            for (int j = 0; j < Size; j++) {
                newGrid[i][j] = nextAcreState(grid, i, j);
            }
        }
        grid = newGrid;
    }

    int remainingMinutes = (1000000000 - cycleStart) % cycleLength;
    for (int i = 0; i < remainingMinutes; i++) {
        vector<string> newGrid = grid;
        for (int i = 0; i < Size; i++) {
            for (int j = 0; j < Size; j++) {
                newGrid[i][j] = nextAcreState(grid, i, j);
            }
        }
        grid = newGrid;
    }

    int wooded = 0, lumberyards = 0;
    for (int i = 0; i < Size; i++) {
        for (int j = 0; j < Size; j++) {
            if (grid[i][j] == Trees) wooded++;
            else if (grid[i][j] == Lumberyard) lumberyards++;
        }
    }

    cout << wooded * lumberyards << endl;

    return 0;
}
