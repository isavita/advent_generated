
#include <iostream>
#include <fstream>
#include <vector>
#include <sstream>

std::vector<int> parseRange(std::string s) {
    std::vector<int> range;
    std::stringstream ss(s);
    std::string token;
    while (std::getline(ss, token, '-')) {
        range.push_back(std::stoi(token));
    }
    return range;
}

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        return 1;
    }

    int count = 0;
    std::string line;
    while (std::getline(file, line)) {
        std::vector<std::string> pair;
        std::stringstream ss(line);
        std::string token;
        while (std::getline(ss, token, ',')) {
            pair.push_back(token);
        }

        // Extract ranges
        std::vector<int> left = parseRange(pair[0]);
        std::vector<int> right = parseRange(pair[1]);

        // Check if ranges overlap
        if (left[0] <= right[1] && left[1] >= right[0]) {
            count++;
        }
    }

    std::cout << count << std::endl;

    return 0;
}
