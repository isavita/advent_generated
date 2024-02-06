
#include <iostream>
#include <fstream>
#include <vector>
#include <unordered_map>
#include <string>
#include <sstream>
#include <numeric>

bool producesClockSignal(int a, const std::vector<std::string>& instructions);

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

    for (int a = 1;; a++) {
        if (producesClockSignal(a, instructions)) {
            std::cout << a << std::endl;
            break;
        }
    }

    return 0;
}

bool producesClockSignal(int a, const std::vector<std::string>& instructions) {
    std::unordered_map<std::string, int> registers = {{"a", a}, {"b", 0}, {"c", 0}, {"d", 0}};
    int lastOutput = 0;
    int outputCount = 0;

    for (size_t i = 0; i < instructions.size();) {
        std::istringstream iss(instructions[i]);
        std::string op;
        iss >> op;

        if (op == "cpy") {
            std::string src, dest;
            iss >> src >> dest;
            int val = (isdigit(src[0]) || src[0] == '-') ? std::stoi(src) : registers[src];
            registers[dest] = val;
        } else if (op == "inc") {
            std::string reg;
            iss >> reg;
            registers[reg]++;
        } else if (op == "dec") {
            std::string reg;
            iss >> reg;
            registers[reg]--;
        } else if (op == "jnz") {
            std::string check;
            int jump;
            iss >> check >> jump;
            int val = (isdigit(check[0]) || check[0] == '-') ? std::stoi(check) : registers[check];
            if (val != 0) {
                i += jump;
                continue;
            }
        } else if (op == "out") {
            std::string val;
            iss >> val;
            int output = (isdigit(val[0]) || val[0] == '-') ? std::stoi(val) : registers[val];
            if (output != 0 && output != 1) {
                return false;
            }
            if (outputCount > 0 && output == lastOutput) {
                return false;
            }
            lastOutput = output;
            outputCount++;
            if (outputCount > 50) {
                return true;
            }
        }
        i++;
    }

    return false;
}
