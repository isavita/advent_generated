#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>

int main() {
    std::ifstream inputFile("input.txt");
    std::string line;
    int checksum = 0;

    while (std::getline(inputFile, line)) {
        std::istringstream iss(line);
        int minVal, maxVal, num;
        iss >> minVal;
        maxVal = minVal;

        while (iss >> num) {
            if (num < minVal) {
                minVal = num;
            }
            if (num > maxVal) {
                maxVal = num;
            }
        }

        checksum += (maxVal - minVal);
    }

    std::cout << checksum << std::endl;

    return 0;
}