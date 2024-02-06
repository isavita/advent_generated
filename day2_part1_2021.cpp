
#include <iostream>
#include <fstream>
#include <string>
#include <sstream>

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cout << "Error opening file" << std::endl;
        return 1;
    }

    int horizontalPosition = 0;
    int depth = 0;
    std::string line;

    while (std::getline(file, line)) {
        std::istringstream iss(line);
        std::string direction;
        int units;
        iss >> direction >> units;

        if (direction == "forward") {
            horizontalPosition += units;
        } else if (direction == "down") {
            depth += units;
        } else if (direction == "up") {
            depth -= units;
        }
    }

    int product = horizontalPosition * depth;
    std::cout << product << std::endl;

    return 0;
}
