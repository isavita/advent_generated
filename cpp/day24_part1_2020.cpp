
#include <iostream>
#include <fstream>
#include <string>
#include <unordered_set>

using namespace std;

struct Point {
    int x, y;
    
    bool operator==(const Point& other) const {
        return x == other.x && y == other.y;
    }
};

namespace std {
    template <>
    struct hash<Point> {
        size_t operator()(const Point& p) const {
            size_t h1 = hash<int>()(p.x);
            size_t h2 = hash<int>()(p.y);
            return h1 ^ (h2 << 1);
        }
    };
}

int main() {
    ifstream inputFile("input.txt");
    string instruction;
    unordered_set<Point> black_tiles;

    while (getline(inputFile, instruction)) {
        int x = 0, y = 0;
        for (size_t i = 0; i < instruction.length();) {
            if (instruction[i] == 'e') {
                x += 1;
                i += 1;
            } else if (instruction[i] == 'w') {
                x -= 1;
                i += 1;
            } else if (instruction.substr(i, 2) == "se") {
                y -= 1;
                i += 2;
            } else if (instruction.substr(i, 2) == "sw") {
                x -= 1;
                y -= 1;
                i += 2;
            } else if (instruction.substr(i, 2) == "ne") {
                x += 1;
                y += 1;
                i += 2;
            } else if (instruction.substr(i, 2) == "nw") {
                y += 1;
                i += 2;
            }
        }

        Point p = {x, y};
        if (black_tiles.count(p)) {
            black_tiles.erase(p);
        } else {
            black_tiles.insert(p);
        }
    }

    cout << black_tiles.size() << endl;

    return 0;
}
