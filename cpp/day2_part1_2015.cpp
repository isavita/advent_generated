
#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <algorithm>

int min(int a, int b, int c) {
    return std::min(std::min(a, b), c);
}

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cerr << "Error opening file" << std::endl;
        return 1;
    }

    int total = 0;
    std::string line;
    while (std::getline(file, line)) {
        std::istringstream iss(line);
        std::string token;
        std::vector<int> dimensions;
        while (std::getline(iss, token, 'x')) {
            dimensions.push_back(std::stoi(token));
        }
        if (dimensions.size() != 3) {
            std::cerr << "Invalid input format" << std::endl;
            return 1;
        }

        int l = dimensions[0];
        int w = dimensions[1];
        int h = dimensions[2];

        int side1 = l * w;
        int side2 = w * h;
        int side3 = h * l;

        int smallest = min(side1, side2, side3);
        total += 2 * side1 + 2 * side2 + 2 * side3 + smallest;
    }

    std::cout << total << std::endl;

    return 0;
}
