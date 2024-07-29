#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <string>
#include <algorithm>

bool isMovingAway(int xPos, int yPos, int xVel, int yVel, int xMin, int xMax, int yMin, int yMax) {
    return (xPos < xMin && xVel < 0) || (xPos > xMax && xVel > 0) || (yPos < yMin && yVel < 0);
}

int main() {
    std::ifstream file("input.txt");
    std::string line;
    std::getline(file, line);
    
    auto pos1 = line.find("x=");
    auto pos2 = line.find(", y=");
    auto xRange = line.substr(pos1 + 2, pos2 - pos1 - 2);
    auto yRange = line.substr(pos2 + 4);
    
    int xMin = std::stoi(xRange.substr(0, xRange.find("..")));
    int xMax = std::stoi(xRange.substr(xRange.find("..") + 2));
    int yMin = std::stoi(yRange.substr(0, yRange.find("..")));
    int yMax = std::stoi(yRange.substr(yRange.find("..") + 2));

    int maxY = std::numeric_limits<int>::min();
    
    for (int xVel = -1000; xVel <= 1000; ++xVel) {
        for (int yVel = -1000; yVel <= 1000; ++yVel) {
            int xPos = 0, yPos = 0, curXVel = xVel, curYVel = yVel, highestY = yPos;
            while (true) {
                xPos += curXVel;
                yPos += curYVel;
                
                if (xPos >= xMin && xPos <= xMax && yPos >= yMin && yPos <= yMax) {
                    maxY = std::max(maxY, highestY);
                    break;
                }
                
                if (isMovingAway(xPos, yPos, curXVel, curYVel, xMin, xMax, yMin, yMax)) {
                    break;
                }
                
                if (curXVel > 0) curXVel--;
                else if (curXVel < 0) curXVel++;
                
                curYVel--;
                highestY = std::max(highestY, yPos);
            }
        }
    }
    
    std::cout << maxY << std::endl;
    return 0;
}