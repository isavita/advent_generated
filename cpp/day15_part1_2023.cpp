#include <iostream>
#include <fstream>
#include <vector>
#include <string>

const int hashTableSize = 256;

struct Step {
    std::string Label;
    int NumBox;
    std::string Operation;
    int Number;
};

int hashString(std::string str) {
    int res = 0;
    for (int i = 0; i < str.length(); i++) {
        char ch = str[i];
        res += int(ch);
        res *= 17;
        res %= hashTableSize;
    }
    return res;
}

Step parseStep(std::string stepStr) {
    Step step;

    step.Label = stepStr.substr(0, stepStr.find_first_of("=-0123456789"));
    step.NumBox = hashString(step.Label);
    step.Operation = stepStr.substr(step.Label.length(), 1);
    if (step.Operation == "=") {
        step.Number = std::stoi(stepStr.substr(step.Label.length() + 1));
    }

    return step;
}

int solve(std::vector<std::string> input) {
    std::string line = input[0];
    std::vector<std::string> steps;
    size_t pos = 0;
    std::string token;
    while ((pos = line.find(",")) != std::string::npos) {
        token = line.substr(0, pos);
        steps.push_back(token);
        line.erase(0, pos + 1);
    }
    steps.push_back(line);

    int res = 0;
    for (std::string step : steps) {
        res += hashString(step);
    }
    return res;
}

std::vector<std::string> readFile(std::string fileName) {
    std::ifstream file(fileName);
    std::vector<std::string> input;
    if (file.is_open()) {
        std::string line;
        while (std::getline(file, line)) {
            input.push_back(line);
        }
        file.close();
    }
    return input;
}

int main() {
    std::vector<std::string> input = readFile("input.txt");
    std::cout << solve(input) << std::endl;
    return 0;
}