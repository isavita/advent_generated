
#include <iostream>
#include <fstream>
#include <string>
#include <cmath>

struct Ship {
    int x, y, facing;
};

void processInstruction(Ship &ship, char action, int value) {
    switch (action) {
        case 'N':
            ship.y += value;
            break;
        case 'S':
            ship.y -= value;
            break;
        case 'E':
            ship.x += value;
            break;
        case 'W':
            ship.x -= value;
            break;
        case 'L':
            ship.facing = (ship.facing - value + 360) % 360;
            break;
        case 'R':
            ship.facing = (ship.facing + value) % 360;
            break;
        case 'F':
            switch (ship.facing) {
                case 0:
                    ship.x += value;
                    break;
                case 90:
                    ship.y -= value;
                    break;
                case 180:
                    ship.x -= value;
                    break;
                case 270:
                    ship.y += value;
                    break;
            }
            break;
    }
}

int abs(int x) {
    return (x < 0) ? -x : x;
}

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cout << "Error opening file" << std::endl;
        return 1;
    }

    Ship ship = {0, 0, 0};
    std::string line;
    while (std::getline(file, line)) {
        char action = line[0];
        int value = std::stoi(line.substr(1));
        processInstruction(ship, action, value);
    }

    int manhattanDistance = abs(ship.x) + abs(ship.y);
    std::cout << manhattanDistance << std::endl;

    return 0;
}
