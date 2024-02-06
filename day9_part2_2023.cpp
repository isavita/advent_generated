
#include <iostream>
#include <fstream>
#include <vector>
#include <sstream>
#include <string>

std::vector<std::vector<int> > parseInput(std::vector<std::string> input) {
    std::vector<std::vector<int> > histories;
    for (const std::string& line : input) {
        std::vector<int> numbers;
        std::istringstream iss(line);
        int number;
        while (iss >> number) {
            numbers.push_back(number);
        }
        histories.push_back(numbers);
    }
    return histories;
}

std::vector<int> parseStringToInts(std::string numbersLine) {
    std::vector<int> numbers;
    std::istringstream iss(numbersLine);
    int number;
    while (iss >> number) {
        numbers.push_back(number);
    }
    return numbers;
}

bool allZeros(std::vector<int> nums) {
    for (int num : nums) {
        if (num != 0) {
            return false;
        }
    }
    return true;
}

std::vector<int> calculateExtrapolation(std::vector<int> history) {
    std::vector<int> extrapolations;
    for (size_t i = 1; i < history.size(); i++) {
        int extrapolation = history[i] - history[i - 1];
        extrapolations.push_back(extrapolation);
    }
    return extrapolations;
}

std::vector<std::vector<int> > calculateExtrapolations(std::vector<int> history) {
    std::vector<std::vector<int> > extrapolationsSeries;
    extrapolationsSeries.push_back(history);

    for (size_t i = 1; i < history.size(); i++) {
        std::vector<int> previousExtrapolations = extrapolationsSeries[i - 1];
        if (allZeros(previousExtrapolations)) {
            return extrapolationsSeries;
        }

        std::vector<int> extrapolations = calculateExtrapolation(previousExtrapolations);
        extrapolationsSeries.push_back(extrapolations);
    }

    return extrapolationsSeries;
}

int solve(std::vector<std::string> input) {
    std::vector<std::vector<int> > histories = parseInput(input);
    int res = 0;

    for (const std::vector<int>& history : histories) {
        std::vector<std::vector<int> > extrapolationsSeries = calculateExtrapolations(history);

        int pastPrediction = 0;
        for (int i = extrapolationsSeries.size() - 1; i > -1; i--) {
            pastPrediction = extrapolationsSeries[i][0] - pastPrediction;
        }

        res += pastPrediction;
    }

    return res;
}

std::vector<std::string> readFile(std::string fileName) {
    std::ifstream file(fileName);
    std::vector<std::string> lines;
    std::string line;
    while (std::getline(file, line)) {
        lines.push_back(line);
    }
    return lines;
}

int main() {
    std::vector<std::string> input = readFile("input.txt");
    std::cout << solve(input) << std::endl;
    return 0;
}
