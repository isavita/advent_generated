
#include <iostream>
#include <fstream>
#include <vector>
#include <sstream>
#include <cmath>

std::vector<int> parseInput(const std::string& line) {
    std::vector<int> values;
    std::istringstream iss(line);
    std::string word;
    while (iss >> word) {
        if (word != "Time:" && word != "Distance:") {
            values.push_back(std::stoi(word));
        }
    }
    return values;
}

int countWaysToWin(int time, int distance) {
    int count = 0;
    for (int holdTime = 1; holdTime < time; ++holdTime) {
        int travelTime = time - holdTime;
        int travelDistance = holdTime * travelTime;
        if (travelDistance > distance) {
            ++count;
        }
    }
    return count;
}

int main() {
    std::ifstream inputFile("input.txt");
    if (!inputFile.is_open()) {
        std::cerr << "Error opening file" << std::endl;
        return 1;
    }

    std::string timeLine, distanceLine;
    std::getline(inputFile, timeLine);
    std::getline(inputFile, distanceLine);

    std::vector<int> times = parseInput(timeLine);
    std::vector<int> distances = parseInput(distanceLine);

    int result = 1;
    for (size_t i = 0; i < times.size(); ++i) {
        int waysToWin = countWaysToWin(times[i], distances[i]);
        result *= waysToWin;
    }

    std::cout << "The result is: " << result << std::endl;

    return 0;
}
