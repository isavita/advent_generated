
#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <sstream>

enum RegName { A = 0, B = 1, C = 2, D = 3 };

enum ArgType { REG, IMM };

struct Arg {
    ArgType type;
    int value;
};

enum Opcode { CPY, INC, DEC, JNZ, TGL };

struct Instruction {
    Opcode op;
    Arg arg1;
    Arg arg2;
};

Arg parseArg(const std::string& s) {
    if (s.length() == 1 && s[0] >= 'a' && s[0] <= 'd') {
        return {REG, s[0] - 'a'};
    }
    return {IMM, std::stoi(s)};
}

std::vector<Instruction> readInstructions(const std::string& filename) {
    std::vector<Instruction> instructions;
    std::ifstream file(filename);
    std::string line;

    while (std::getline(file, line)) {
        std::stringstream ss(line);
        std::string opcode_str;
        ss >> opcode_str;

        Instruction instr;
        if (opcode_str == "cpy") {
            instr.op = CPY;
            std::string s1, s2;
            ss >> s1 >> s2;
            instr.arg1 = parseArg(s1);
            instr.arg2 = parseArg(s2);
        } else if (opcode_str == "inc") {
            instr.op = INC;
            std::string s1;
            ss >> s1;
            instr.arg1 = parseArg(s1);
        } else if (opcode_str == "dec") {
            instr.op = DEC;
            std::string s1;
            ss >> s1;
            instr.arg1 = parseArg(s1);
        } else if (opcode_str == "jnz") {
            instr.op = JNZ;
            std::string s1, s2;
            ss >> s1 >> s2;
            instr.arg1 = parseArg(s1);
            instr.arg2 = parseArg(s2);
        } else if (opcode_str == "tgl") {
            instr.op = TGL;
            std::string s1;
            ss >> s1;
            instr.arg1 = parseArg(s1);
        }
        instructions.push_back(instr);
    }
    return instructions;
}

void toggleInstruction(Instruction& instr) {
    switch (instr.op) {
        case INC: instr.op = DEC; break;
        case DEC:
        case TGL: instr.op = INC; break;
        case JNZ: instr.op = CPY; break;
        case CPY: instr.op = JNZ; break;
    }
}

void executeInstructions(std::vector<Instruction>& instructions, int registers[]) {
    long long pc = 0;

    auto get_val = [&](const Arg& arg) {
        return arg.type == REG ? registers[arg.value] : arg.value;
    };

    while (pc >= 0 && pc < instructions.size()) {
        Instruction current_instr = instructions[pc];

        switch (current_instr.op) {
            case CPY:
                if (current_instr.arg2.type == REG) {
                    registers[current_instr.arg2.value] = get_val(current_instr.arg1);
                }
                break;
            case INC:
                if (current_instr.arg1.type == REG) {
                    registers[current_instr.arg1.value]++;
                }
                break;
            case DEC:
                if (current_instr.arg1.type == REG) {
                    registers[current_instr.arg1.value]--;
                }
                break;
            case JNZ:
                if (get_val(current_instr.arg1) != 0) {
                    pc += get_val(current_instr.arg2);
                    pc--;
                }
                break;
            case TGL: {
                long long target_pc = pc + get_val(current_instr.arg1);
                if (target_pc >= 0 && target_pc < instructions.size()) {
                    toggleInstruction(instructions[target_pc]);
                }
                break;
            }
        }
        pc++;
    }
}

int main() {
    std::vector<Instruction> instructions = readInstructions("input.txt");
    int registers[4] = {7, 0, 0, 0};

    executeInstructions(instructions, registers);
    std::cout << registers[A] << std::endl;

    return 0;
}
