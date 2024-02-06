
#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <unordered_map>

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

std::unordered_map<int, std::vector<std::unordered_map<std::string, int>>> getBoxes(std::vector<std::string> stepsStr) {
    std::unordered_map<int, std::vector<std::unordered_map<std::string, int>>> boxes;

    for (std::string stepStr : stepsStr) {
        Step step = parseStep(stepStr);
        std::vector<std::unordered_map<std::string, int>>& boxContents = boxes[step.NumBox];

        switch (step.Operation[0]) {
            case '-':
                for (int i = 0; i < boxContents.size(); i++) {
                    if (boxContents[i].count(step.Label)) {
                        boxContents.erase(boxContents.begin() + i);
                        break;
                    }
                }
                break;
            case '=':
                bool found = false;
                for (auto& content : boxContents) {
                    if (content.count(step.Label)) {
                        content[step.Label] = step.Number;
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    boxContents.push_back({{step.Label, step.Number}});
                }
                break;
        }

        if (boxContents.empty()) {
            boxes.erase(step.NumBox);
        } else {
            boxes[step.NumBox] = boxContents;
        }
    }

    return boxes;
}

std::string toStringBoxes(std::unordered_map<int, std::vector<std::unordered_map<std::string, int>>> boxes) {
    std::string res = "";

    for (int iBox = 0; iBox < hashTableSize; iBox++) {
        if (boxes.count(iBox)) {
            res += "Box " + std::to_string(iBox) + " : ";
            for (auto& content : boxes[iBox]) {
                for (auto& entry : content) {
                    res += "[" + entry.first + " " + std::to_string(entry.second) + "] ";
                }
            }
            res += "\n";
        }
    }

    return res;
}

int calculatePower(std::unordered_map<int, std::vector<std::unordered_map<std::string, int>>> boxes) {
    int res = 0;

    for (int iBox = 0; iBox < hashTableSize; iBox++) {
        if (boxes.count(iBox)) {
            for (int iSlot = 0; iSlot < boxes[iBox].size(); iSlot++) {
                for (auto& entry : boxes[iBox][iSlot]) {
                    res += (iBox + 1) * (iSlot + 1) * entry.second;
                }
            }
        }
    }

    return res;
}

int solve(std::vector<std::string> input) {
    std::string line = input[0];
    std::vector<std::string> stepsStr;
    std::stringstream ss(line);
    std::string token;
    while (std::getline(ss, token, ',')) {
        stepsStr.push_back(token);
    }

    auto boxes = getBoxes(stepsStr);

    return calculatePower(boxes);
}

std::vector<std::string> readFile(std::string fileName) {
    std::ifstream file(fileName);
    std::vector<std::string> lines;
    std::string line;
    while (std::getline(file, line)) {
        lines.push_back(line);
    }
    file.close();
    return lines;
}

int main() {
    std::vector<std::string> input = readFile("input.txt");
    std::cout << solve(input) << std::endl;
    return 0;
}
