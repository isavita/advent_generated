#include <iostream>
#include <fstream>
#include <vector>
#include <string>

using namespace std;

vector<string> readInput() {
    ifstream file("input.txt");
    vector<string> seats;
    string line;
    while (getline(file, line)) {
        seats.push_back(line);
    }
    return seats;
}

int countAdjacentOccupied(vector<string> seats, int i, int j) {
    int count = 0;
    for (int x = -1; x <= 1; x++) {
        for (int y = -1; y <= 1; y++) {
            if (x == 0 && y == 0) continue;
            int nx = i + x;
            int ny = j + y;
            if (nx >= 0 && nx < seats.size() && ny >= 0 && ny < seats[0].size()) {
                if (seats[nx][ny] == '#') count++;
            }
        }
    }
    return count;
}

vector<string> applyRules(vector<string> seats) {
    vector<string> newSeats = seats;
    for (int i = 0; i < seats.size(); i++) {
        for (int j = 0; j < seats[0].size(); j++) {
            if (seats[i][j] == '.') continue;
            int adjacentOccupied = countAdjacentOccupied(seats, i, j);
            if (seats[i][j] == 'L' && adjacentOccupied == 0) {
                newSeats[i][j] = '#';
            } else if (seats[i][j] == '#' && adjacentOccupied >= 4) {
                newSeats[i][j] = 'L';
            }
        }
    }
    return newSeats;
}

bool hasChanged(vector<string> seats, vector<string> newSeats) {
    for (int i = 0; i < seats.size(); i++) {
        for (int j = 0; j < seats[0].size(); j++) {
            if (seats[i][j] != newSeats[i][j]) return true;
        }
    }
    return false;
}

int countOccupied(vector<string> seats) {
    int count = 0;
    for (string row : seats) {
        for (char c : row) {
            if (c == '#') count++;
        }
    }
    return count;
}

int main() {
    vector<string> seats = readInput();
    vector<string> newSeats = seats;
    while (true) {
        newSeats = applyRules(seats);
        if (!hasChanged(seats, newSeats)) break;
        seats = newSeats;
    }
    cout << "Occupied seats: " << countOccupied(seats) << endl;
    return 0;
}