#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <map>
#include <climits>

using namespace std;

struct Point {
    int x, y;
};

map<Point, int> getPointsWithSteps(string path) {
    map<Point, int> points;
    Point current = {0, 0};
    int steps = 0;
    stringstream ss(path);
    string move;
    while (getline(ss, move, ',')) {
        char dir = move[0];
        int dist = stoi(move.substr(1));
        for (int i = 0; i < dist; i++) {
            steps++;
            switch (dir) {
                case 'U':
                    current.y++;
                    break;
                case 'D':
                    current.y--;
                    break;
                case 'L':
                    current.x--;
                    break;
                case 'R':
                    current.x++;
                    break;
            }
            if (points.find(current) == points.end()) {
                points[current] = steps;
            }
        }
    }
    return points;
}

int main() {
    ifstream file("input.txt");
    string line1, line2;
    getline(file, line1);
    getline(file, line2);
    map<Point, int> wire1 = getPointsWithSteps(line1);
    map<Point, int> wire2 = getPointsWithSteps(line2);

    int minSteps = INT_MAX;
    for (auto p : wire1) {
        if (wire2.find(p.first) != wire2.end()) {
            int totalSteps = p.second + wire2[p.first];
            if (totalSteps < minSteps) {
                minSteps = totalSteps;
            }
        }
    }

    cout << minSteps << endl;

    return 0;
}