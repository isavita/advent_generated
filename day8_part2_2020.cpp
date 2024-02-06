#include <iostream>
#include <fstream>
#include <vector>
#include <unordered_map>
#include <string>
#include <sstream>

std::pair<std::string, int> parseInstruction(std::string instruction) {
    std::istringstream iss(instruction);
    std::string op;
    int arg;
    iss >> op >> arg;
    return std::make_pair(op, arg);
}

std::pair<int, bool> executeBootCode(std::vector<std::string> instructions) {
    int accumulator = 0;
    std::unordered_map<int, bool> visited;
    int currentInstruction = 0;

    while (currentInstruction < instructions.size()) {
        if (visited[currentInstruction]) {
            return std::make_pair(accumulator, false);
        }

        visited[currentInstruction] = true;
        auto [op, arg] = parseInstruction(instructions[currentInstruction]);

        if (op == "acc") {
            accumulator += arg;
            currentInstruction++;
        } else if (op == "jmp") {
            currentInstruction += arg;
        } else {
            currentInstruction++;
        }
    }

    return std::make_pair(accumulator, true);
}

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cout << "Error opening file" << std::endl;
        return 1;
    }

    std::vector<std::string> instructions;
    std::string line;
    while (std::getline(file, line)) {
        instructions.push_back(line);
    }

    for (int i = 0; i < instructions.size(); i++) {
        auto [op, arg] = parseInstruction(instructions[i]);
        if (op == "acc") {
            continue;
        }

        std::vector<std::string> modifiedInstructions = instructions;
        if (op == "jmp") {
            modifiedInstructions[i] = "nop " + std::to_string(arg);
        } else {
            modifiedInstructions[i] = "jmp " + std::to_string(arg);
        }

        auto [accumulator, terminated] = executeBootCode(modifiedInstructions);
        if (terminated) {
            std::cout << accumulator << std::endl;
            break;
        }
    }

    return 0;
}