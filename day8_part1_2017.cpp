#include <iostream>
#include <fstream>
#include <sstream>
#include <map>

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cout << "File reading error" << std::endl;
        return 1;
    }

    std::map<std::string, int> registers;

    std::string line;
    while (std::getline(file, line)) {
        std::istringstream iss(line);
        std::string reg, op, condReg, condOp;
        int amount, condVal;

        iss >> reg >> op >> amount;
        iss.ignore(3); // Ignore "if "
        iss >> condReg >> condOp >> condVal;

        bool cond = false;
        if (condOp == ">") {
            cond = registers[condReg] > condVal;
        } else if (condOp == ">=") {
            cond = registers[condReg] >= condVal;
        } else if (condOp == "<") {
            cond = registers[condReg] < condVal;
        } else if (condOp == "<=") {
            cond = registers[condReg] <= condVal;
        } else if (condOp == "==") {
            cond = registers[condReg] == condVal;
        } else if (condOp == "!=") {
            cond = registers[condReg] != condVal;
        }

        if (cond) {
            if (op == "inc") {
                registers[reg] += amount;
            } else if (op == "dec") {
                registers[reg] -= amount;
            }
        }
    }

    int maxValue = 0;
    for (const auto& pair : registers) {
        if (pair.second > maxValue) {
            maxValue = pair.second;
        }
    }

    std::cout << maxValue << std::endl;

    return 0;
}