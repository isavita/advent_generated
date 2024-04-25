#include <fstream>
#include <map>
#include <string>
#include <vector>

using namespace std;

struct point {
    int x, y;
};

int abs(int x) {
    return x < 0 ? -x : x;
}

int main() {
    ifstream file("input.txt");
    point head = {0, 0}, tail = {0, 0};
    map<point, bool> visited;
    visited[tail] = true;

    string dir, steps;
    while (file >> dir >> steps) {
        int numSteps = stoi(steps);
        for (int i = 0; i < numSteps; i++) {
            if (dir == "R") head.x++;
            else if (dir == "L") head.x--;
            else if (dir == "U") head.y++;
            else if (dir == "D") head.y--;

            if (abs(head.x - tail.x) > 1 || abs(head.y - tail.y) > 1) {
                if (head.x != tail.x && head.y != tail.y) {
                    if (head.x > tail.x) tail.x++;
                    else tail.x--;
                    if (head.y > tail.y) tail.y++;
                    else tail.y--;
                } else {
                    if (head.x > tail.x) tail.x++;
                    else if (head.x < tail.x) tail.x--;
                    if (head.y > tail.y) tail.y++;
                    else if (head.y < tail.y) tail.y--;
                }
            }

            visited[tail] = true;
        }
    }

    cout << visited.size() << endl;

    return 0;
}