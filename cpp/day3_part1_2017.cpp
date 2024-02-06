#include <iostream>
#include <fstream>
#include <cmath>

int main() {
    std::ifstream inputFile("input.txt");
    int target;
    inputFile >> target;

    int sideLength = std::ceil(std::sqrt(target));
    if (sideLength % 2 == 0) {
        sideLength++;
    }

    int maxValue = sideLength * sideLength;
    int stepsFromEdge = (sideLength - 1) / 2;
    int distanceToMiddle = 0;

    for (int i = 0; i < 4; i++) {
        int middlePoint = maxValue - stepsFromEdge - (sideLength - 1) * i;
        int distance = std::abs(target - middlePoint);
        if (distance < distanceToMiddle || i == 0) {
            distanceToMiddle = distance;
        }
    }

    int manhattanDistance = stepsFromEdge + distanceToMiddle;

    std::cout << manhattanDistance << std::endl;

    return 0;
}