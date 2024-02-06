#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <cmath>

int getMode(int instruction, int position) {
    return instruction / static_cast<int>(std::pow(10, position + 1)) % 10;
}

int getParam(std::vector<int>& program, int pointer, int mode) {
    if (mode == 0) {
        return program[program[pointer]];
    }
    return program[pointer];
}

int runProgram(std::vector<int>& program, int input) {
    int output = 0;
    for (int pointer = 0; pointer < program.size();) {
        int instruction = program[pointer];
        int opcode = instruction % 100;

        switch (opcode) {
            case 1:
            case 2: {
                int param1 = getParam(program, pointer + 1, getMode(instruction, 1));
                int param2 = getParam(program, pointer + 2, getMode(instruction, 2));
                int result = (opcode == 1) ? param1 + param2 : param1 * param2;
                program[program[pointer + 3]] = result;
                pointer += 4;
                break;
            }
            case 3:
                program[program[pointer + 1]] = input;
                pointer += 2;
                break;
            case 4:
                output = getParam(program, pointer + 1, getMode(instruction, 1));
                pointer += 2;
                break;
            case 99:
                return output;
            default:
                throw std::invalid_argument("Unknown opcode: " + std::to_string(opcode));
        }
    }
    return output;
}

int main() {
    std::ifstream inputFile("input.txt");
    if (!inputFile.is_open()) {
        std::cerr << "Error opening file" << std::endl;
        return 1;
    }

    std::string line;
    std::vector<int> program;
    while (std::getline(inputFile, line, ',')) {
        program.push_back(std::stoi(line));
    }

    std::cout << runProgram(program, 1) << std::endl;

    return 0;
}