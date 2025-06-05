
#include <fstream>
#include <iostream>
#include <map>
#include <sstream>
#include <string>
#include <vector>

bool isRegister(char c) {
    return c >= 'a' && c <= 'd';
}

int getValue(const std::string& s, const std::map<char, int>& registers) {
    if (s.length() == 1 && isRegister(s[0])) {
        return registers.at(s[0]);
    } else {
        return std::stoi(s);
    }
}

std::vector<std::string> splitString(const std::string& s) {
    std::istringstream iss(s);
    std::vector<std::string> parts;
    std::string part;
    while (iss >> part) {
        parts.push_back(part);
    }
    return parts;
}

void executeProgram(std::vector<std::string>& instructions, std::map<char, int>& registers) {
    long long i = 0;
    while (i < instructions.size()) {
        if (i + 5 < instructions.size()) {
            std::vector<std::string> p0 = splitString(instructions[i]);
            std::vector<std::string> p1 = splitString(instructions[i + 1]);
            std::vector<std::string> p2 = splitString(instructions[i + 2]);
            std::vector<std::string> p3 = splitString(instructions[i + 3]);
            std::vector<std::string> p4 = splitString(instructions[i + 4]);
            std::vector<std::string> p5 = splitString(instructions[i + 5]);

            if (p0[0] == "cpy" && p1[0] == "inc" && p2[0] == "dec" &&
                p3[0] == "jnz" && p4[0] == "dec" && p5[0] == "jnz") {
                
                std::string cpy_x_str = p0[1];
                char cpy_y_char = p0[2][0];
                char inc_a_char = p1[1][0];
                char dec_c_char = p2[1][0];
                std::string jnz_c_str = p3[1];
                int jnz_c_offset = std::stoi(p3[2]);
                char dec_d_char = p4[1][0];
                std::string jnz_d_str = p5[1];
                int jnz_d_offset = std::stoi(p5[2]);

                if (inc_a_char == 'a' && dec_c_char == cpy_y_char &&
                    jnz_c_str.length() == 1 && jnz_c_str[0] == cpy_y_char && jnz_c_offset == -2 &&
                    dec_d_char == 'd' &&
                    jnz_d_str.length() == 1 && jnz_d_str[0] == 'd' && jnz_d_offset == -5) {
                    
                    if (isRegister(cpy_y_char)) {
                        registers['a'] += getValue(cpy_x_str, registers) * registers['d'];
                        registers[cpy_y_char] = 0;
                        registers['d'] = 0;
                        i += 6;
                        continue;
                    }
                }
            }
        }

        std::vector<std::string> parts = splitString(instructions[i]);
        std::string cmd = parts[0];

        if (cmd == "tgl") {
            int x = getValue(parts[1], registers);
            long long target_idx = i + x;
            if (target_idx >= 0 && target_idx < instructions.size()) {
                std::vector<std::string> target_parts = splitString(instructions[target_idx]);
                if (target_parts.size() == 2) {
                    if (target_parts[0] == "inc") {
                        target_parts[0] = "dec";
                    } else {
                        target_parts[0] = "inc";
                    }
                } else if (target_parts.size() == 3) {
                    if (target_parts[0] == "jnz") {
                        target_parts[0] = "cpy";
                    } else {
                        target_parts[0] = "jnz";
                    }
                }
                instructions[target_idx] = target_parts[0];
                for (size_t k = 1; k < target_parts.size(); ++k) {
                    instructions[target_idx] += " " + target_parts[k];
                }
            }
            i += 1;
            continue;
        }

        if (cmd == "cpy") {
            std::string x_str = parts[1];
            char y_char = parts[2][0];
            if (isRegister(y_char)) {
                registers[y_char] = getValue(x_str, registers);
            }
            i += 1;
        } else if (cmd == "inc") {
            char x_char = parts[1][0];
            if (isRegister(x_char)) {
                registers[x_char]++;
            }
            i += 1;
        } else if (cmd == "dec") {
            char x_char = parts[1][0];
            if (isRegister(x_char)) {
                registers[x_char]--;
            }
            i += 1;
        } else if (cmd == "jnz") {
            std::string x_str = parts[1];
            std::string y_str = parts[2];
            if (getValue(x_str, registers) != 0) {
                i += getValue(y_str, registers);
            } else {
                i += 1;
            }
        } else {
            i += 1;
        }
    }
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::vector<std::string> instructions;
    std::ifstream inputFile("input.txt");
    std::string line;
    while (std::getline(inputFile, line)) {
        instructions.push_back(line);
    }
    inputFile.close();

    std::map<char, int> registers;
    registers['a'] = 12;
    registers['b'] = 0;
    registers['c'] = 0;
    registers['d'] = 0;

    executeProgram(instructions, registers);

    std::cout << registers['a'] << std::endl;

    return 0;
}
