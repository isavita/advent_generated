
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <algorithm>

int abs(int x) {
    return x < 0 ? -x : x;
}

int max(int a, int b) {
    return a > b ? a : b;
}

int distance(int x, int y, int z) {
    return (abs(x) + abs(y) + abs(z)) / 2;
}

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cout << "File reading error" << std::endl;
        return 1;
    }

    std::string input;
    std::getline(file, input);

    std::vector<std::string> directions;
    size_t pos = 0;
    std::string token;
    while ((pos = input.find(',')) != std::string::npos) {
        token = input.substr(0, pos);
        directions.push_back(token);
        input.erase(0, pos + 1);
    }
    directions.push_back(input);

    int x = 0, y = 0, z = 0;
    int maxDistance = 0;

    for (const auto& dir : directions) {
        if (dir == "n") {
            y++;
            z--;
        } else if (dir == "ne") {
            x++;
            z--;
        } else if (dir == "se") {
            x++;
            y--;
        } else if (dir == "s") {
            y--;
            z++;
        } else if (dir == "sw") {
            x--;
            z++;
        } else if (dir == "nw") {
            x--;
            y++;
        }

        int curDistance = distance(x, y, z);
        maxDistance = std::max(maxDistance, curDistance);
    }

    std::cout << distance(x, y, z) << std::endl;

    return 0;
}
