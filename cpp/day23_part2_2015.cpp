
#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <stdexcept> // For stoi

// No explicit parsing function needed, handled inline

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::ifstream file("input.txt");
    std::vector<std::string> instructions;
    std::string line;
    while (std::getline(file, line)) {
        if (!line.empty()) {
             instructions.push_back(line);
        }
    }

    unsigned long long reg_a = 1; // Per problem description (Part 2 setup)
    unsigned long long reg_b = 0;

    size_t pc = 0; // Program counter

    while (pc < instructions.size()) {
        const std::string& instruction = instructions[pc];
        std::string opcode = instruction.substr(0, 3);
        char reg_name = ' ';
        int offset = 0;

        try {
            if (opcode[0] == 'j') { // jmp, jie, jio
                 if (opcode == "jmp") {
                     offset = std::stoi(instruction.substr(4));
                 } else { // jie, jio
                     reg_name = instruction[4];
                     offset = std::stoi(instruction.substr(7)); // Skip "X, "
                 }
            } else { // hlf, tpl, inc
                 reg_name = instruction[4];
            }
        } catch (const std::invalid_argument& e) {
            // Handle potential stoi errors if input might be malformed
            std::cerr << "Error parsing instruction: " << instruction << std::endl;
             return 1;
        } catch (const std::out_of_range& e) {
            // Handle potential stoi errors if input might be malformed
             std::cerr << "Error parsing offset (out of range): " << instruction << std::endl;
             return 1;
        }


        unsigned long long* target_reg = nullptr;
        if (reg_name == 'a') {
            target_reg = &reg_a;
        } else if (reg_name == 'b') {
            target_reg = &reg_b;
        }
        // else: target_reg remains nullptr for jmp

        if (opcode == "hlf") {
            *target_reg /= 2;
            pc++;
        } else if (opcode == "tpl") {
            *target_reg *= 3;
            pc++;
        } else if (opcode == "inc") {
            (*target_reg)++;
            pc++;
        } else if (opcode == "jmp") {
            pc += offset;
        } else if (opcode == "jie") {
            if (*target_reg % 2 == 0) {
                pc += offset;
            } else {
                pc++;
            }
        } else if (opcode == "jio") {
             if (*target_reg == 1) {
                 pc += offset;
             } else {
                 pc++;
             }
        } else {
             // Should not happen with valid input based on Python code
             std::cerr << "Unknown instruction: " << instruction << std::endl;
             return 1;
        }
    }

    std::cout << reg_b << std::endl;

    return 0;
}
