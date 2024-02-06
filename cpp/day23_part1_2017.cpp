#include <iostream>
#include <fstream>
#include <vector>
#include <unordered_map>
#include <string>
#include <sstream>

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cerr << "Error opening file" << std::endl;
        return 1;
    }

    int mulCount = 0;
    int pointer = 0;
    std::unordered_map<std::string, int> registers;
    std::vector<std::string> instructions;

    std::string line;
    while (std::getline(file, line)) {
        instructions.push_back(line);
    }

    auto getValue = [&](std::string s) -> int {
        try {
            return std::stoi(s);
        } catch (const std::invalid_argument&) {
            return registers[s];
        }
    };

    while (pointer >= 0 && pointer < instructions.size()) {
        std::istringstream iss(instructions[pointer]);
        std::string cmd, x, y;
        iss >> cmd >> x >> y;

        if (cmd == "set") {
            registers[x] = getValue(y);
        } else if (cmd == "sub") {
            registers[x] -= getValue(y);
        } else if (cmd == "mul") {
            registers[x] *= getValue(y);
            mulCount++;
        } else if (cmd == "jnz") {
            if (getValue(x) != 0) {
                pointer += getValue(y) - 1;
            }
        }
        pointer++;
    }

    std::cout << mulCount << std::endl;

    return 0;
}