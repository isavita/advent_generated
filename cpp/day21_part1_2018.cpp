
#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <sstream>
#include <array>
#include <map>

enum Opcode {
    ADDR, ADDI,
    MULR, MULI,
    BANR, BANI,
    BORR, BORI,
    SETR, SETI,
    GTIR, GTRI, GTRR,
    EQIR, EQRI, EQRR,
    UNKNOWN_OPCODE
};

std::map<std::string, Opcode> opcodeMap = {
    {"addr", ADDR}, {"addi", ADDI},
    {"mulr", MULR}, {"muli", MULI},
    {"banr", BANR}, {"bani", BANI},
    {"borr", BORR}, {"bori", BORI},
    {"setr", SETR}, {"seti", SETI},
    {"gtir", GTIR}, {"gtri", GTRI}, {"gtrr", GTRR},
    {"eqir", EQIR}, {"eqri", EQRI}, {"eqrr", EQRR},
};

struct Instruction {
    Opcode op;
    std::array<int, 3> abc;
};

class OpcodeComputer {
public:
    std::vector<Instruction> instructions;
    std::array<int, 6> registers;
    int instructionPointerReg;

    OpcodeComputer(const std::vector<Instruction>& insts, int ipReg)
        : instructions(insts), instructionPointerReg(ipReg) {
        registers.fill(0);
    }

    bool tick() {
        int ip = registers[instructionPointerReg];

        if (ip < 0 || ip >= static_cast<int>(instructions.size())) {
            return true;
        }

        const Instruction& inst = instructions[ip];
        const int a = inst.abc[0];
        const int b = inst.abc[1];
        const int c = inst.abc[2];

        switch (inst.op) {
            case ADDR: registers[c] = registers[a] + registers[b]; break;
            case ADDI: registers[c] = registers[a] + b; break;
            case MULR: registers[c] = registers[a] * registers[b]; break;
            case MULI: registers[c] = registers[a] * b; break;
            case BANR: registers[c] = registers[a] & registers[b]; break;
            case BANI: registers[c] = registers[a] & b; break;
            case BORR: registers[c] = registers[a] | registers[b]; break;
            case BORI: registers[c] = registers[a] | b; break;
            case SETR: registers[c] = registers[a]; break;
            case SETI: registers[c] = a; break;
            case GTIR: registers[c] = (a > registers[b]) ? 1 : 0; break;
            case GTRI: registers[c] = (registers[a] > b) ? 1 : 0; break;
            case GTRR: registers[c] = (registers[a] > registers[b]) ? 1 : 0; break;
            case EQIR: registers[c] = (a == registers[b]) ? 1 : 0; break;
            case EQRI: registers[c] = (registers[a] == b) ? 1 : 0; break;
            case EQRR: registers[c] = (registers[a] == registers[b]) ? 1 : 0; break;
            case UNKNOWN_OPCODE: return true;
        }

        registers[instructionPointerReg]++;

        return false;
    }
};

OpcodeComputer parseInput(const std::string& input_data) {
    std::stringstream ss(input_data);
    std::string line;
    int instruction_pointer = -1;
    std::vector<Instruction> instructions;

    if (std::getline(ss, line)) {
        std::stringstream line_ss(line);
        std::string token;
        line_ss >> token >> instruction_pointer;
    }

    while (std::getline(ss, line)) {
        if (line.empty()) continue;
        std::stringstream line_ss(line);
        std::string name;
        int a, b, c;
        line_ss >> name >> a >> b >> c;

        Instruction inst;
        inst.op = opcodeMap.count(name) ? opcodeMap[name] : UNKNOWN_OPCODE;
        inst.abc = {a, b, c};
        instructions.push_back(inst);
    }

    return OpcodeComputer(instructions, instruction_pointer);
}

long long solve(const std::string& input_data) {
    OpcodeComputer opcode_computer = parseInput(input_data);

    while (!opcode_computer.tick()) {
        if (opcode_computer.registers[opcode_computer.instructionPointerReg] == 28) {
            return opcode_computer.registers[5];
        }
    }
    return -1;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cerr << "Error opening input.txt" << std::endl;
        return 1;
    }
    std::stringstream buffer;
    buffer << file.rdbuf();
    std::string input_data = buffer.str();

    std::cout << solve(input_data) << std::endl;

    return 0;
}
