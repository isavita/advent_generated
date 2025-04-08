
#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <sstream>

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::ifstream file("input.txt");
    std::vector<std::string> instructions;
    std::string line;

    if (!file.is_open()) {
        // Error handling is usually excluded based on the prompt,
        // but it's good practice. Silently exit if file not found.
        return 1;
    }

    while (std::getline(file, line)) {
        if (!line.empty()) { // Avoid empty lines
            instructions.push_back(line);
        }
    }
    file.close();

    unsigned long long reg_a = 0;
    unsigned long long reg_b = 0;
    long long pc = 0; // Use signed type for potential negative jumps/offsets

    while (pc >= 0 && pc < static_cast<long long>(instructions.size())) {
        std::stringstream ss(instructions[pc]);
        std::string opcode;
        std::string arg1_str, arg2_str;

        ss >> opcode;

        long long next_pc_offset = 1; // Default offset is +1

        if (opcode == "hlf") {
            ss >> arg1_str;
            if (arg1_str[0] == 'a') reg_a /= 2;
            else reg_b /= 2;
        } else if (opcode == "tpl") {
            ss >> arg1_str;
            if (arg1_str[0] == 'a') reg_a *= 3;
            else reg_b *= 3;
        } else if (opcode == "inc") {
            ss >> arg1_str;
            if (arg1_str[0] == 'a') reg_a++;
            else reg_b++;
        } else if (opcode == "jmp") {
            ss >> arg1_str;
            next_pc_offset = std::stoll(arg1_str);
        } else if (opcode == "jie") {
            ss >> arg1_str >> arg2_str; // Reads "a," or "b," then offset
            char reg = arg1_str[0];
            bool condition = (reg == 'a') ? (reg_a % 2 == 0) : (reg_b % 2 == 0);
            if (condition) {
                next_pc_offset = std::stoll(arg2_str);
            }
        } else if (opcode == "jio") {
            ss >> arg1_str >> arg2_str; // Reads "a," or "b," then offset
            char reg = arg1_str[0];
            bool condition = (reg == 'a') ? (reg_a == 1) : (reg_b == 1);
            if (condition) {
                next_pc_offset = std::stoll(arg2_str);
            }
        }
        // else: unknown instruction, just moves to next line (offset remains 1)

        pc += next_pc_offset;
    }

    std::cout << reg_b << std::endl;

    return 0;
}
