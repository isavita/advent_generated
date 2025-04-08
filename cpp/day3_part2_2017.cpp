
#include <iostream>
#include <fstream>
#include <unordered_map>

using namespace std;

int main() {
    ifstream file("input.txt");
    int target;
    file >> target;

    unordered_map<long long, int> grid;
    grid[0] = 1;

    int x = 0, y = 0;
    int dx = 0, dy = -1;

    while (true) {
        if (x == y || (x < 0 && x == -y) || (x > 0 && x == 1 - y)) {
            int temp = dx;
            dx = -dy;
            dy = temp;
        }

        x += dx;
        y += dy;

        int value = 0;
        for (int i = -1; i <= 1; ++i) {
            for (int j = -1; j <= 1; ++j) {
                long long nx = x + i;
                long long ny = y + j;
                long long key = (nx << 32) | (ny & 0xFFFFFFFF);
                if (grid.count(key)) {
                    value += grid[key];
                }
            }
        }

        long long key = ((long long)x << 32) | (y & 0xFFFFFFFF);
        grid[key] = value;

        if (value > target) {
            cout << value << endl;
            break;
        }
    }

    return 0;
}
