
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <unordered_map>

using namespace std;

int main() {
    ifstream file("input.txt");
    if (!file.is_open()) {
        cerr << "Error opening file" << endl;
        return 1;
    }

    unordered_map<long long, bool> points;
    string line;
    bool readingPoints = true;

    while (getline(file, line)) {
        if (line.empty()) {
            readingPoints = false;
            continue;
        }

        if (readingPoints) {
            stringstream ss(line);
            string x_str, y_str;
            getline(ss, x_str, ',');
            getline(ss, y_str);
            int x = stoi(x_str);
            int y = stoi(y_str);
            points[((long long)x << 32) | y] = true;
        } else {
            size_t pos = line.find("=");
            char axis = line[pos - 1];
            int value = stoi(line.substr(pos + 1));

            unordered_map<long long, bool> newPoints;
            if (axis == 'x') {
                for (auto const& [point_encoded, _] : points) {
                    int x = point_encoded >> 32;
                    int y = point_encoded & 0xFFFFFFFF;
                    if (x > value) {
                        x = 2 * value - x;
                    }
                    newPoints[((long long)x << 32) | y] = true;
                }
            } else {
                for (auto const& [point_encoded, _] : points) {
                    int x = point_encoded >> 32;
                    int y = point_encoded & 0xFFFFFFFF;
                    if (y > value) {
                        y = 2 * value - y;
                    }
                    newPoints[((long long)x << 32) | y] = true;
                }
            }

            cout << newPoints.size() << endl;
            return 0;
        }
    }

    return 0;
}
