#include <iostream>
#include <fstream>
#include <vector>

int main() {
    std::ifstream file("input.txt");
    std::vector<int> offsets;
    int offset;
    while (file >> offset) {
        offsets.push_back(offset);
    }

    int index = 0;
    int steps = 0;

    while (index >= 0 && index < offsets.size()) {
        int jump = offsets[index];
        if (jump >= 3) {
            offsets[index]--;
        } else {
            offsets[index]++;
        }
        index += jump;
        steps++;
    }

    std::cout << steps << std::endl;

    return 0;
}