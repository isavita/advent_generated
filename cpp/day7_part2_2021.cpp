#include <fstream>
#include <vector>
#include <algorithm>
#include <iostream>
#include <sstream>
#include <cmath>

using namespace std;

int calculateNewFuel(int currentPosition, int newPosition) {
    int diff = abs(currentPosition - newPosition);
    return (diff * (diff + 1)) / 2;
}

int main() {
    ifstream file("input.txt");
    if (!file) {
        cerr << "Error opening file" << endl;
        return 1;
    }

    vector<int> positions;
    string line;
    while (getline(file, line)) {
        stringstream ss(line);
        string num_str;
        while (getline(ss, num_str, ',')) {
            positions.push_back(stoi(num_str));
        }
    }

    sort(positions.begin(), positions.end());

    int min_fuel = INT_MAX;
    for (int i = positions[0]; i <= positions.back(); i++) {
        int fuel = 0;
        for (int pos : positions) {
            fuel += calculateNewFuel(pos, i);
        }
        if (fuel < min_fuel) {
            min_fuel = fuel;
        }
    }
    cout << min_fuel << endl;

    return 0;
}