#include <iostream>
#include <fstream>
#include <string>

int main() {
    std::ifstream inputFile("input.txt");
    if (!inputFile.is_open()) {
        std::cout << "File reading error" << std::endl;
        return 1;
    }

    int steps;
    inputFile >> steps;
    inputFile.close();

    int currentPos = 0;
    int valueAfterZero = 0;

    for (int i = 1; i <= 50000000; i++) {
        currentPos = (currentPos + steps) % i;
        if (currentPos == 0) {
            valueAfterZero = i;
        }
        currentPos++;
    }

    std::cout << valueAfterZero << std::endl;

    return 0;
}