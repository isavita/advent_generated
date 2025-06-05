
#include <iostream>
#include <vector>
#include <string>
#include <map>
#include <functional>
#include <fstream>
#include <sstream>
#include <algorithm>
#include <cmath>

namespace Ops {
long long addr(const std::vector<long long>& r, int a, int b) { return r[a] + r[b]; }
long long addi(const std::vector<long long>& r, int a, int b) { return r[a] + b; }
long long mulr(const std::vector<long long>& r, int a, int b) { return r[a] * r[b]; }
long long muli(const std::vector<long long>& r, int a, int b) { return r[a] * b; }
long long banr(const std::vector<long long>& r, int a, int b) { return r[a] & r[b]; }
long long bani(const std::vector<long long>& r, int a, int b) { return r[a] & b; }
long long borr(const std::vector<long long>& r, int a, int b) { return r[a] | r[b]; }
long long bori(const std::vector<long long>& r, int a, int b) { return r[a] | b; }
long long setr(const std::vector<long long>& r, int a, int b) { return r[a]; }
long long seti(const std::vector<long long>& r, int a, int b) { return a; }
long long gtir(const std::vector<long long>& r, int a, int b) { return a > r[b] ? 1 : 0; }
long long gtri(const std::vector<long long>& r, int a, int b) { return r[a] > b ? 1 : 0; }
long long gtrr(const std::vector<long long>& r, int a, int b) { return r[a] > r[b] ? 1 : 0; }
long long eqir(const std::vector<long long>& r, int a, int b) { return a == r[b] ? 1 : 0; }
long long eqri(const std::vector<long long>& r, int a, int b) { return r[a] == b ? 1 : 0; }
long long eqrr(const std::vector<long long>& r, int a, int b) { return r[a] == r[b] ? 1 : 0; }
}

struct Instruction {
    using OpFunc = std::function<long long(const std::vector<long long>&, int, int)>;
    OpFunc func;
    int A, B, C;
};

const std::map<std::string, Instruction::OpFunc>& get_instruction_map() {
    static const std::map<std::string, Instruction::OpFunc> instruction_map_instance = {
        {"addr", Ops::addr}, {"addi", Ops::addi}, {"mulr", Ops::mulr}, {"muli", Ops::muli},
        {"banr", Ops::banr}, {"bani", Ops::bani}, {"borr", Ops::borr}, {"bori", Ops::bori},
        {"setr", Ops::setr}, {"seti", Ops::seti}, {"gtir", Ops::gtir}, {"gtri", Ops::gtri},
        {"gtrr", Ops::gtrr}, {"eqir", Ops::eqir}, {"eqri", Ops::eqri}, {"eqrr", Ops::eqrr}
    };
    return instruction_map_instance;
}

std::pair<int, std::vector<Instruction>> load_program(const std::vector<std::string>& lines) {
    int ip_register = 0;
    std::vector<Instruction> program;
    const auto& instruction_map = get_instruction_map();

    for (const std::string& line : lines) {
        if (line.empty()) continue;

        std::istringstream iss(line);
        std::string opcode_str;
        iss >> opcode_str;

        if (opcode_str == "#ip") {
            iss >> ip_register;
            continue;
        }

        int a, b, c;
        iss >> a >> b >> c;
        program.push_back({instruction_map.at(opcode_str), a, b, c});
    }
    return {ip_register, program};
}

std::vector<long long> run_program(int ip_register, const std::vector<Instruction>& program, std::vector<long long> registers, int max_cycles) {
    long long ip = 0;
    long long cycles = 0;

    while (ip >= 0 && ip < program.size()) {
        registers[ip_register] = ip;
        const Instruction& current_instruction = program[ip];
        registers[current_instruction.C] = current_instruction.func(registers, current_instruction.A, current_instruction.B);
        ip = registers[ip_register] + 1;
        cycles++;
        if (max_cycles > 0 && cycles >= max_cycles) {
            break;
        }
    }
    return registers;
}

long long sum_divisors_optimized(long long n) {
    long long total = 0;
    for (long long i = 1; i * i <= n; ++i) {
        if (n % i == 0) {
            total += i;
            if (i * i != n) {
                total += n / i;
            }
        }
    }
    return total;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::ifstream file("input.txt");
    std::vector<std::string> lines;
    std::string line;
    while (std::getline(file, line)) {
        lines.push_back(line);
    }
    file.close();

    auto [ip_register, program] = load_program(lines);

    std::vector<long long> registers = {0, 0, 0, 0, 0, 0};
    registers[0] = 1;

    registers = run_program(ip_register, program, registers, 1000);

    long long n = 0;
    if (!registers.empty()) {
        n = *std::max_element(registers.begin(), registers.end());
    }

    long long total = sum_divisors_optimized(n);

    std::cout << total << std::endl;

    return 0;
}
