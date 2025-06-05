
#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <sstream>
#include <map>
#include <deque>
#include <utility>
#include <stdexcept>

class IntcodeComputer {
public:
    std::map<long long, long long> memory;
    long long ip;
    long long relative_base;
    std::deque<long long> inputs;
    std::deque<long long> outputs;
    bool halted;
    bool needs_input;

    IntcodeComputer(const std::vector<long long>& program, std::deque<long long> initial_inputs = {})
        : ip(0), relative_base(0), inputs(std::move(initial_inputs)), halted(false), needs_input(false) {
        for (size_t i = 0; i < program.size(); ++i) {
            memory[i] = program[i];
        }
    }

    long long get_param_value(int mode, long long offset) {
        long long address_or_value_at_offset = memory[ip + offset];
        if (mode == 0) { // Position mode
            return memory[address_or_value_at_offset];
        } else if (mode == 1) { // Immediate mode
            return address_or_value_at_offset;
        } else if (mode == 2) { // Relative mode
            return memory[relative_base + address_or_value_at_offset];
        } else {
            throw std::runtime_error("Unknown parameter mode for read");
        }
    }

    long long get_param_address(int mode, long long offset) {
        long long address_or_value_at_offset = memory[ip + offset];
        if (mode == 0) { // Position mode
            return address_or_value_at_offset;
        } else if (mode == 2) { // Relative mode
            return relative_base + address_or_value_at_offset;
        } else {
            throw std::runtime_error("Unknown parameter mode for write");
        }
    }

    void run() {
        while (true) {
            long long instruction = memory[ip];
            int opcode = instruction % 100;
            int mode1 = (instruction / 100) % 10;
            int mode2 = (instruction / 1000) % 10;
            int mode3 = (instruction / 10000) % 10;

            if (opcode == 99) {
                halted = true;
                break;
            } else if (opcode == 1) { // ADD
                long long p1 = get_param_value(mode1, 1);
                long long p2 = get_param_value(mode2, 2);
                long long dest_addr = get_param_address(mode3, 3);
                memory[dest_addr] = p1 + p2;
                ip += 4;
            } else if (opcode == 2) { // MUL
                long long p1 = get_param_value(mode1, 1);
                long long p2 = get_param_value(mode2, 2);
                long long dest_addr = get_param_address(mode3, 3);
                memory[dest_addr] = p1 * p2;
                ip += 4;
            } else if (opcode == 3) { // INPUT
                if (inputs.empty()) {
                    needs_input = true;
                    return;
                }
                needs_input = false;
                long long value = inputs.front();
                inputs.pop_front();
                long long dest_addr = get_param_address(mode1, 1);
                memory[dest_addr] = value;
                ip += 2;
            } else if (opcode == 4) { // OUTPUT
                long long p1 = get_param_value(mode1, 1);
                outputs.push_back(p1);
                ip += 2;
                if (outputs.size() == 3) {
                    return;
                }
            } else if (opcode == 5) { // JUMP-IF-TRUE
                long long p1 = get_param_value(mode1, 1);
                long long p2 = get_param_value(mode2, 2);
                if (p1 != 0) {
                    ip = p2;
                } else {
                    ip += 3;
                }
            } else if (opcode == 6) { // JUMP-IF-FALSE
                long long p1 = get_param_value(mode1, 1);
                long long p2 = get_param_value(mode2, 2);
                if (p1 == 0) {
                    ip = p2;
                } else {
                    ip += 3;
                }
            } else if (opcode == 7) { // LESS THAN
                long long p1 = get_param_value(mode1, 1);
                long long p2 = get_param_value(mode2, 2);
                long long dest_addr = get_param_address(mode3, 3);
                memory[dest_addr] = (p1 < p2) ? 1 : 0;
                ip += 4;
            } else if (opcode == 8) { // EQUALS
                long long p1 = get_param_value(mode1, 1);
                long long p2 = get_param_value(mode2, 2);
                long long dest_addr = get_param_address(mode3, 3);
                memory[dest_addr] = (p1 == p2) ? 1 : 0;
                ip += 4;
            } else if (opcode == 9) { // ADJUST RELATIVE BASE
                long long p1 = get_param_value(mode1, 1);
                relative_base += p1;
                ip += 2;
            } else {
                throw std::runtime_error("Unknown opcode: " + std::to_string(opcode));
            }
        }
    }
};

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::vector<long long> program;
    std::ifstream inputFile("input.txt");
    std::string line;
    std::getline(inputFile, line);
    std::stringstream ss(line);
    std::string segment;
    while(std::getline(ss, segment, ',')) {
        program.push_back(std::stoll(segment));
    }

    std::vector<IntcodeComputer> computers;
    for (int i = 0; i < 50; ++i) {
        computers.emplace_back(program, std::deque<long long>{(long long)i});
    }

    std::vector<std::deque<std::pair<long long, long long>>> packetQueues(50);
    bool first_packet_to_255_found = false;

    while (true) {
        for (int i = 0; i < 50; ++i) {
            IntcodeComputer& computer = computers[i];

            if (!packetQueues[i].empty()) {
                long long x = packetQueues[i].front().first;
                long long y = packetQueues[i].front().second;
                packetQueues[i].pop_front();
                computer.inputs.push_back(x);
                computer.inputs.push_back(y);
            } else {
                computer.inputs.push_back(-1);
            }

            computer.run();

            while (computer.outputs.size() >= 3) {
                long long dest = computer.outputs[0];
                long long x = computer.outputs[1];
                long long y = computer.outputs[2];

                computer.outputs.erase(computer.outputs.begin(), computer.outputs.begin() + 3);

                if (dest == 255) {
                    if (!first_packet_to_255_found) {
                        std::cout << y << std::endl;
                        first_packet_to_255_found = true;
                        return 0;
                    }
                } else if (dest >= 0 && dest < 50) {
                    packetQueues[dest].emplace_back(x, y);
                }
            }
        }
    }

    return 0;
}
