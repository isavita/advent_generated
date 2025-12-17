
#include <fstream>
#include <iostream>
#include <string>

int main() {
    std::ifstream in("input.txt");
    char dir;
    int dist;
    int pos = 50;
    int part1 = 0, part2 = 0;

    while (in >> dir >> dist) {
        int step = (dir == 'R') ? 1 : -1;
        while (dist--) {
            pos = (pos + step + 100) % 100;
            part2 += (pos == 0);
        }
        part1 += (pos == 0);
    }
    std::cout << part1 << '\n' << part2 << '\n';
}
