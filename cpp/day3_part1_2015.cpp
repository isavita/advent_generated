
#include <iostream>
#include <fstream>
#include <string>
#include <unordered_set>

using namespace std;

struct Point {
    int x;
    int y;

    bool operator==(const Point& other) const {
        return x == other.x && y == other.y;
    }
};

namespace std {
    template <>
    struct hash<Point> {
        size_t operator()(const Point& p) const {
            return ((hash<int>()(p.x) ^ (hash<int>()(p.y) << 1)) >> 1);
        }
    };
}

int main() {
    ifstream inputFile("input.txt");
    if (!inputFile.is_open()) {
        cerr << "Error opening input.txt" << endl;
        return 1;
    }

    string directions;
    getline(inputFile, directions);
    inputFile.close();

    unordered_set<Point> visitedHouses;
    Point currentPos = {0, 0};
    visitedHouses.insert(currentPos);

    for (char dir : directions) {
        switch (dir) {
            case '^':
                currentPos.y++;
                break;
            case 'v':
                currentPos.y--;
                break;
            case '>':
                currentPos.x++;
                break;
            case '<':
                currentPos.x--;
                break;
        }
        visitedHouses.insert(currentPos);
    }

    cout << visitedHouses.size() << endl;

    return 0;
}
