
#include <iostream>
#include <fstream>
#include <sstream>
#include <unordered_map>
#include <vector>
#include <numeric>

std::unordered_map<std::string, int> registers = {{"a", 0}, {"b", 0}, {"c", 0}, {"d", 0}};

void executeInstructions(const std::vector<std::string>& instructions, std::unordered_map<std::string, int>& registers);

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

    executeInstructions(instructions, registers);

    std::cout << registers["a"] << std::endl;

    return 0;
}

void executeInstructions(const std::vector<std::string>& instructions, std::unordered_map<std::string, int>& registers) {
    for (size_t i = 0; i < instructions.size();) {
        std::istringstream iss(instructions[i]);
        std::vector<std::string> parts(std::istream_iterator<std::string>{iss}, std::istream_iterator<std::string>());

        if (parts[0] == "cpy") {
            int val = (isdigit(parts[1][0])) ? std::stoi(parts[1]) : registers[parts[1]];
            registers[parts[2]] = val;
            i++;
        } else if (parts[0] == "inc") {
            registers[parts[1]]++;
            i++;
        } else if (parts[0] == "dec") {
            registers[parts[1]]--;
            i++;
        } else if (parts[0] == "jnz") {
            int val = (isdigit(parts[1][0])) ? std::stoi(parts[1]) : registers[parts[1]];
            if (val != 0) {
                int jump = std::stoi(parts[2]);
                i += jump;
            } else {
                i++;
            }
        }
    }
}
