#include <iostream>
#include <fstream>
#include <sstream>

bool isValidTriangle(int a, int b, int c) {
    return a + b > c && a + c > b && b + c > a;
}

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cerr << "Error opening file" << std::endl;
        return 1;
    }

    std::string line;
    int validTriangles = 0;
    while (std::getline(file, line)) {
        std::istringstream iss(line);
        int a, b, c;
        if (!(iss >> a >> b >> c)) {
            std::cerr << "Invalid input format" << std::endl;
            continue;
        }

        if (isValidTriangle(a, b, c)) {
            validTriangles++;
        }
    }

    std::cout << validTriangles << std::endl;

    return 0;
}