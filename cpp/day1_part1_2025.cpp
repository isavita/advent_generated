
#include <fstream>
#include <iostream>

int main() {
    std::ifstream in("input.txt");
    char dir;
    int steps, pos = 50, zeros = 0;
    while (in >> dir >> steps) {
        if (dir == 'L') pos = (pos - steps + 100) % 100;
        else pos = (pos + steps) % 100;
        if (pos == 0) ++zeros;
    }
    std::cout << zeros << '\n';
}
