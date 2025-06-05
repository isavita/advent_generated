
#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <functional>
#include <cstdio>
#include <sstream>

void addr(std::vector<int>& reg, int A, int B, int C) { reg[C] = reg[A] + reg[B]; }
void addi(std::vector<int>& reg, int A, int B, int C) { reg[C] = reg[A] + B; }
void mulr(std::vector<int>& reg, int A, int B, int C) { reg[C] = reg[A] * reg[B]; }
void muli(std::vector<int>& reg, int A, int B, int C) { reg[C] = reg[A] * B; }
void banr(std::vector<int>& reg, int A, int B, int C) { reg[C] = reg[A] & reg[B]; }
void bani(std::vector<int>& reg, int A, int B, int C) { reg[C] = reg[A] & B; }
void borr(std::vector<int>& reg, int A, int B, int C) { reg[C] = reg[A] | reg[B]; }
void bori(std::vector<int>& reg, int A, int B, int C) { reg[C] = reg[A] | B; }
void setr(std::vector<int>& reg, int A, int B, int C) { reg[C] = reg[A]; }
void seti(std::vector<int>& reg, int A, int B, int C) { reg[C] = A; }
void gtir(std::vector<int>& reg, int A, int B, int C) { reg[C] = (A > reg[B]) ? 1 : 0; }
void gtri(std::vector<int>& reg, int A, int B, int C) { reg[C] = (reg[A] > B) ? 1 : 0; }
void gtrr(std::vector<int>& reg, int A, int B, int C) { reg[C] = (reg[A] > reg[B]) ? 1 : 0; }
void eqir(std::vector<int>& reg, int A, int B, int C) { reg[C] = (A == reg[B]) ? 1 : 0; }
void eqri(std::vector<int>& reg, int A, int B, int C) { reg[C] = (reg[A] == B) ? 1 : 0; }
void eqrr(std::vector<int>& reg, int A, int B, int C) { reg[C] = (reg[A] == reg[B]) ? 1 : 0; }

struct Sample {
    std::vector<int> before;
    std::vector<int> instruction;
    std::vector<int> after;
};

std::vector<int> parseRegistersFromLine(const std::string& line) {
    std::vector<int> regs(4);
    size_t bracket_start = line.find('[');
    if (bracket_start == std::string::npos) return regs;
    sscanf(line.c_str() + bracket_start, "[%d, %d, %d, %d]", &regs[0], &regs[1], &regs[2], &regs[3]);
    return regs;
}

std::vector<int> parseInstructionLine(const std::string& line) {
    std::vector<int> instr(4);
    std::stringstream ss(line);
    ss >> instr[0] >> instr[1] >> instr[2] >> instr[3];
    return instr;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::vector<std::function<void(std::vector<int>&, int, int, int)>> operations = {
        addr, addi, mulr, muli, banr, bani, borr, bori, setr, seti, gtir, gtri, gtrr, eqir, eqri, eqrr
    };

    std::vector<Sample> samples;
    std::ifstream file("input.txt");
    std::string line;

    if (!file.is_open()) return 1;

    while (std::getline(file, line)) {
        if (line.find("Before:") == 0) {
            Sample s;
            s.before = parseRegistersFromLine(line);

            std::getline(file, line);
            s.instruction = parseInstructionLine(line);

            std::getline(file, line);
            s.after = parseRegistersFromLine(line);

            samples.push_back(s);

            std::getline(file, line); // Consume blank line
        }
    }
    file.close();

    int count = 0;
    for (const auto& sample : samples) {
        int matches = 0;
        int A = sample.instruction[1];
        int B = sample.instruction[2];
        int C = sample.instruction[3];

        for (const auto& op_func : operations) {
            std::vector<int> registers = sample.before;
            op_func(registers, A, B, C);
            if (registers == sample.after) {
                matches++;
            }
        }
        if (matches >= 3) {
            count++;
        }
    }

    std::cout << count << std::endl;

    return 0;
}
