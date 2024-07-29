#include <iostream>
#include <fstream>
#include <string>
#include <cstdlib>

struct Ship {
    int x = 0, y = 0, waypointX = 10, waypointY = 1;

    void processInstruction(char action, int value) {
        switch (action) {
            case 'N': waypointY += value; break;
            case 'S': waypointY -= value; break;
            case 'E': waypointX += value; break;
            case 'W': waypointX -= value; break;
            case 'L': rotateWaypoint(-value); break;
            case 'R': rotateWaypoint(value); break;
            case 'F': x += waypointX * value; y += waypointY * value; break;
        }
    }

    void rotateWaypoint(int degrees) {
        degrees = (degrees + 360) % 360;
        if (degrees == 90 || degrees == -270) {
            std::swap(waypointX, waypointY);
            waypointY = -waypointY;
        } else if (degrees == 180 || degrees == -180) {
            waypointX = -waypointX;
            waypointY = -waypointY;
        } else if (degrees == 270 || degrees == -90) {
            std::swap(waypointX, waypointY);
            waypointX = -waypointX;
        }
    }
};

int main() {
    std::ifstream file("input.txt");
    Ship ship;
    std::string line;

    while (std::getline(file, line)) {
        char action = line[0];
        int value = std::stoi(line.substr(1));
        ship.processInstruction(action, value);
    }

    std::cout << std::abs(ship.x) + std::abs(ship.y) << std::endl;
    return 0;
}