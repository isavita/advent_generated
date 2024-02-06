#include <iostream>
#include <fstream>
#include <sstream>
#include <unordered_map>
#include <vector>
#include <numeric>

void executeInstructions(std::vector<std::string> instructions, std::unordered_map<std::string, int>& registers);

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cerr << "Error opening file" << std::endl;
        return 1;
    }

    std::vector<std::string> instructions;
    std::string line;
    while (std::getline(file, line)) {
        instructions.push_back(line);
    }

    std::unordered_map<std::string, int> registers = {{"a", 0}, {"b", 0}, {"c", 1}, {"d", 0}};
    executeInstructions(instructions, registers);

    std::cout << registers["a"] << std::endl;

    return 0;
}

void executeInstructions(std::vector<std::string> instructions, std::unordered_map<std::string, int>& registers) {
    for (int i = 0; i < instructions.size();) {
        std::istringstream iss(instructions[i]);
        std::string instruction;
        iss >> instruction;

        if (instruction == "cpy") {
            std::string source, dest;
            iss >> source >> dest;
            int val = (std::isdigit(source[0]) || source[0] == '-') ? std::stoi(source) : registers[source];
            registers[dest] = val;
            i++;
        } else if (instruction == "inc") {
            std::string reg;
            iss >> reg;
            registers[reg]++;
            i++;
        } else if (instruction == "dec") {
            std::string reg;
            iss >> reg;
            registers[reg]--;
            i++;
        } else if (instruction == "jnz") {
            std::string check;
            int jump;
            iss >> check >> jump;
            int val = (std::isdigit(check[0]) || check[0] == '-') ? std::stoi(check) : registers[check];
            if (val != 0) {
                i += jump;
            } else {
                i++;
            }
        }
    }
}