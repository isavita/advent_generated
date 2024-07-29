#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <set>

bool isMovingAway(int xPos, int yPos, int xVel, int yVel, int xMin, int xMax, int yMin, int yMax) {
    return (xPos < xMin && xVel < 0) || (xPos > xMax && xVel > 0) || (yPos < yMin && yVel < 0);
}

int main() {
    std::ifstream file("input.txt");
    std::string line;
    std::getline(file, line);
    
    int xMin, xMax, yMin, yMax;
    sscanf(line.c_str(), "target area: x=%d..%d, y=%d..%d", &xMin, &xMax, &yMin, &yMax);
    
    std::set<std::string> velocities;
    for (int xVel = -1000; xVel <= 1000; ++xVel) {
        for (int yVel = -1000; yVel <= 1000; ++yVel) {
            int xPos = 0, yPos = 0, curXVel = xVel, curYVel = yVel;
            bool inTargetArea = false;
            while (true) {
                xPos += curXVel;
                yPos += curYVel;
                if (xPos >= xMin && xPos <= xMax && yPos >= yMin && yPos <= yMax) {
                    inTargetArea = true;
                    break;
                }
                if (isMovingAway(xPos, yPos, curXVel, curYVel, xMin, xMax, yMin, yMax)) {
                    break;
                }
                if (curXVel > 0) curXVel--;
                else if (curXVel < 0) curXVel++;
                curYVel--;
            }
            if (inTargetArea) {
                velocities.insert(std::to_string(xVel) + "," + std::to_string(yVel));
            }
        }
    }
    std::cout << velocities.size() << std::endl;
}