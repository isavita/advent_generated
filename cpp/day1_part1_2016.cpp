#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>

struct Position {
    int x, y, dirIndex;
};

int abs(int x) {
    if (x < 0) {
        return -x;
    }
    return x;
}

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cerr << "Error opening file." << std::endl;
        return 1;
    }

    std::vector<std::string> instructions;
    std::string line;
    while (std::getline(file, line)) {
        std::istringstream iss(line);
        std::string instruction;
        while (iss >> instruction) {
            instructions.push_back(instruction);
        }
    }

    Position pos = {0, 0, 0};
    int directions[4][2] = {{0, 1}, {1, 0}, {0, -1}, {-1, 0}};

    for (const auto& instruction : instructions) {
        char turn = instruction[0];
        int blocks = std::stoi(instruction.substr(1));

        if (turn == 'R') {
            pos.dirIndex = (pos.dirIndex + 1) % 4;
        } else {
            pos.dirIndex = (pos.dirIndex - 1 + 4) % 4;
        }

        pos.x += directions[pos.dirIndex][0] * blocks;
        pos.y += directions[pos.dirIndex][1] * blocks;
    }

    std::cout << abs(pos.x) + abs(pos.y) << std::endl;

    return 0;
}