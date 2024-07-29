#include <iostream>
#include <fstream>
#include <string>
#include <unordered_map>
#include <sstream>
#include <vector>

long long getValue(const std::string &token, const std::unordered_map<char, long long> &registers) {
    if (std::isalpha(token[0])) {
        return registers.count(token[0]) ? registers.at(token[0]) : 0;
    }
    return std::stoll(token);
}

int main() {
    std::ifstream infile("input.txt");
    std::string line;
    std::unordered_map<char, long long> registers;
    long long lastSound = 0;
    size_t instructionPointer = 0;
    std::vector<std::string> instructions;

    while (std::getline(infile, line)) {
        instructions.push_back(line);
    }

    while (instructionPointer < instructions.size()) {
        std::istringstream iss(instructions[instructionPointer]);
        std::string command;
        iss >> command;

        if (command == "snd") {
            char reg;
            iss >> reg;
            lastSound = registers.count(reg) ? registers[reg] : reg - '0';
        } else if (command == "set") {
            char reg;
            std::string valueStr;
            iss >> reg >> valueStr;
            registers[reg] = getValue(valueStr, registers);
        } else if (command == "add") {
            char reg;
            std::string valueStr;
            iss >> reg >> valueStr;
            registers[reg] += getValue(valueStr, registers);
        } else if (command == "mul") {
            char reg;
            std::string valueStr;
            iss >> reg >> valueStr;
            registers[reg] *= getValue(valueStr, registers);
        } else if (command == "mod") {
            char reg;
            std::string valueStr;
            iss >> reg >> valueStr;
            registers[reg] %= getValue(valueStr, registers);
        } else if (command == "rcv") {
            char reg;
            iss >> reg;
            if (getValue(std::string(1, reg), registers) != 0) {
                std::cout << lastSound << std::endl;
                return 0;
            }
        } else if (command == "jgz") {
            char reg;
            std::string valueStr;
            iss >> reg >> valueStr;
            if (getValue(std::string(1, reg), registers) > 0) {
                instructionPointer += getValue(valueStr, registers);
                continue;
            }
        }

        instructionPointer++;
    }

    return 0;
}