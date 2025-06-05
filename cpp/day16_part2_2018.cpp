
#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <sstream>
#include <map>
#include <set>
#include <algorithm>

// Define Opcode functions
// Registers are passed by reference to be modified
void addr(std::vector<int>& registers, int A, int B, int C) {
    registers[C] = registers[A] + registers[B];
}
void addi(std::vector<int>& registers, int A, int B, int C) {
    registers[C] = registers[A] + B;
}
void mulr(std::vector<int>& registers, int A, int B, int C) {
    registers[C] = registers[A] * registers[B];
}
void muli(std::vector<int>& registers, int A, int B, int C) {
    registers[C] = registers[A] * B;
}
void banr(std::vector<int>& registers, int A, int B, int C) {
    registers[C] = registers[A] & registers[B];
}
void bani(std::vector<int>& registers, int A, int B, int C) {
    registers[C] = registers[A] & B;
}
void borr(std::vector<int>& registers, int A, int B, int C) {
    registers[C] = registers[A] | registers[B];
}
void bori(std::vector<int>& registers, int A, int B, int C) {
    registers[C] = registers[A] | B;
}
void setr(std::vector<int>& registers, int A, int B, int C) {
    registers[C] = registers[A];
}
void seti(std::vector<int>& registers, int A, int B, int C) {
    registers[C] = A;
}
void gtir(std::vector<int>& registers, int A, int B, int C) {
    registers[C] = (A > registers[B]) ? 1 : 0;
}
void gtri(std::vector<int>& registers, int A, int B, int C) {
    registers[C] = (registers[A] > B) ? 1 : 0;
}
void gtrr(std::vector<int>& registers, int A, int B, int C) {
    registers[C] = (registers[A] > registers[B]) ? 1 : 0;
}
void eqir(std::vector<int>& registers, int A, int B, int C) {
    registers[C] = (A == registers[B]) ? 1 : 0;
}
void eqri(std::vector<int>& registers, int A, int B, int C) {
    registers[C] = (registers[A] == B) ? 1 : 0;
}
void eqrr(std::vector<int>& registers, int A, int B, int C) {
    registers[C] = (registers[A] == registers[B]) ? 1 : 0;
}

// Type alias for opcode function pointer
using OpFunc = void(*)(std::vector<int>&, int, int, int);

// Global vector of all opcode functions
std::vector<OpFunc> all_opcodes = {
    addr, addi, mulr, muli, banr, bani, borr, bori,
    setr, seti, gtir, gtri, gtrr, eqir, eqri, eqrr
};

struct Sample {
    std::vector<int> before;
    std::vector<int> instruction;
    std::vector<int> after;
};

std::vector<int> parse_bracketed_list(const std::string& line) {
    std::vector<int> nums;
    size_t start = line.find('[');
    size_t end = line.find(']');
    if (start != std::string::npos && end != std::string::npos) {
        std::string s_to_parse = line.substr(start + 1, end - start - 1);
        std::stringstream ss(s_to_parse);
        std::string segment;
        while (std::getline(ss, segment, ',')) {
            nums.push_back(std::stoi(segment));
        }
    }
    return nums;
}

std::vector<int> parse_instruction_line(const std::string& line) {
    std::vector<int> nums;
    std::stringstream ss(line);
    int num;
    while (ss >> num) {
        nums.push_back(num);
    }
    return nums;
}

bool test_opcode(const Sample& sample, OpFunc opcode_func) {
    std::vector<int> registers = sample.before;
    opcode_func(registers, sample.instruction[1], sample.instruction[2], sample.instruction[3]);
    return registers == sample.after;
}

std::vector<Sample> parse_samples(std::ifstream& file) {
    std::vector<Sample> samples;
    std::string line;

    while (std::getline(file, line) && !line.empty()) {
        Sample s;
        s.before = parse_bracketed_list(line);

        std::getline(file, line);
        s.instruction = parse_instruction_line(line);

        std::getline(file, line);
        s.after = parse_bracketed_list(line);

        samples.push_back(s);

        std::getline(file, line);
        if (file.eof()) break;
    }
    return samples;
}

std::vector<std::vector<int>> parse_program(std::ifstream& file) {
    std::vector<std::vector<int>> program_instructions;
    std::string line;
    while (std::getline(file, line)) {
        if (line.empty()) continue;
        program_instructions.push_back(parse_instruction_line(line));
    }
    return program_instructions;
}

int part_one(const std::vector<Sample>& samples) {
    int count = 0;
    for (const auto& sample : samples) {
        int num_behave_like = 0;
        for (OpFunc opcode_func : all_opcodes) {
            if (test_opcode(sample, opcode_func)) {
                num_behave_like++;
            }
        }
        if (num_behave_like >= 3) {
            count++;
        }
    }
    return count;
}

int part_two(const std::vector<Sample>& samples, const std::vector<std::vector<int>>& program) {
    std::map<int, std::set<int>> possible_opcodes_idx;

    for (int i = 0; i < 16; ++i) {
        possible_opcodes_idx[i] = {};
    }

    for (const auto& sample : samples) {
        int opcode_num = sample.instruction[0];
        for (int i = 0; i < all_opcodes.size(); ++i) {
            if (test_opcode(sample, all_opcodes[i])) {
                possible_opcodes_idx[opcode_num].insert(i);
            }
        }
    }

    std::map<int, int> opcode_mapping;
    
    std::map<int, std::set<int>> current_possible_opcodes = possible_opcodes_idx;

    while (opcode_mapping.size() < 16) {
        int found_opcode_num = -1;
        int found_opcode_idx = -1;

        for (const auto& pair : current_possible_opcodes) {
            if (pair.second.size() == 1) {
                found_opcode_num = pair.first;
                found_opcode_idx = *pair.second.begin();
                break;
            }
        }
        
        opcode_mapping[found_opcode_num] = found_opcode_idx;

        for (auto& pair : current_possible_opcodes) {
            if (pair.first != found_opcode_num) {
                pair.second.erase(found_opcode_idx);
            }
        }
        current_possible_opcodes.erase(found_opcode_num);
    }

    std::vector<int> registers = {0, 0, 0, 0};
    for (const auto& instruction : program) {
        OpFunc actual_opcode = all_opcodes[opcode_mapping[instruction[0]]];
        actual_opcode(registers, instruction[1], instruction[2], instruction[3]);
    }

    return registers[0];
}

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cerr << "Error opening input.txt" << std::endl;
        return 1;
    }

    std::vector<Sample> samples = parse_samples(file);
    std::vector<std::vector<int>> program = parse_program(file);

    std::cout << part_one(samples) << std::endl;
    std::cout << part_two(samples, program) << std::endl;

    file.close();
    return 0;
}
