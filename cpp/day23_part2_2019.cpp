
#include <iostream>
#include <vector>
#include <string>
#include <sstream>
#include <deque>
#include <map>
#include <optional>
#include <fstream>
#include <stdexcept>

class IntcodeComputer {
public:
    std::map<long long, long long> memory;
    long long ip;
    long long relative_base;
    std::deque<long long> inputs;
    std::vector<long long> outputs;
    bool halted;
    bool idle;

    IntcodeComputer(const std::vector<long long>& program, long long address)
        : ip(0), relative_base(0), halted(false), idle(false) {
        for (size_t i = 0; i < program.size(); ++i) {
            memory[i] = program[i];
        }
        inputs.push_back(address);
    }

    long long get_param(int mode, int offset) {
        long long val_addr = ip + offset;
        long long effective_addr;

        if (mode == 0) {
            effective_addr = memory[val_addr];
        } else if (mode == 1) {
            effective_addr = val_addr;
        } else if (mode == 2) {
            effective_addr = relative_base + memory[val_addr];
        } else {
            throw std::runtime_error("Unknown read mode: " + std::to_string(mode));
        }
        return memory[effective_addr];
    }

    void set_param(int mode, int offset, long long value) {
        long long val_addr = ip + offset;
        long long effective_addr;

        if (mode == 0) {
            effective_addr = memory[val_addr];
        } else if (mode == 2) {
            effective_addr = relative_base + memory[val_addr];
        } else {
            throw std::runtime_error("Unknown write mode: " + std::to_string(mode));
        }
        memory[effective_addr] = value;
    }

    void run() {
        idle = false;

        while (true) {
            long long instruction = memory[ip];
            int opcode = instruction % 100;
            int mode1 = (instruction / 100) % 10;
            int mode2 = (instruction / 1000) % 10;
            int mode3 = (instruction / 10000) % 10;

            if (opcode == 99) {
                halted = true;
                break;
            } else if (opcode == 1) {
                long long p1 = get_param(mode1, 1);
                long long p2 = get_param(mode2, 2);
                set_param(mode3, 3, p1 + p2);
                ip += 4;
            } else if (opcode == 2) {
                long long p1 = get_param(mode1, 1);
                long long p2 = get_param(mode2, 2);
                set_param(mode3, 3, p1 * p2);
                ip += 4;
            } else if (opcode == 3) {
                if (inputs.empty()) {
                    set_param(mode1, 1, -1);
                    ip += 2;
                    idle = true;
                    return;
                } else {
                    long long value = inputs.front();
                    inputs.pop_front();
                    set_param(mode1, 1, value);
                    ip += 2;
                }
            } else if (opcode == 4) {
                long long p1 = get_param(mode1, 1);
                outputs.push_back(p1);
                ip += 2;
                if (outputs.size() == 3) {
                    return;
                }
            } else if (opcode == 5) {
                long long p1 = get_param(mode1, 1);
                long long p2 = get_param(mode2, 2);
                if (p1 != 0) {
                    ip = p2;
                } else {
                    ip += 3;
                }
            } else if (opcode == 6) {
                long long p1 = get_param(mode1, 1);
                long long p2 = get_param(mode2, 2);
                if (p1 == 0) {
                    ip = p2;
                } else {
                    ip += 3;
                }
            } else if (opcode == 7) {
                long long p1 = get_param(mode1, 1);
                long long p2 = get_param(mode2, 2);
                set_param(mode3, 3, (p1 < p2) ? 1 : 0);
                ip += 4;
            } else if (opcode == 8) {
                long long p1 = get_param(mode1, 1);
                long long p2 = get_param(mode2, 2);
                set_param(mode3, 3, (p1 == p2) ? 1 : 0);
                ip += 4;
            } else if (opcode == 9) {
                long long p1 = get_param(mode1, 1);
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
    std::string line;
    std::ifstream inputFile("input.txt");

    if (!inputFile.is_open()) {
        std::cerr << "Error opening input.txt" << std::endl;
        return 1;
    }

    std::getline(inputFile, line);
    std::stringstream ss(line);
    std::string segment;

    while (std::getline(ss, segment, ',')) {
        program.push_back(std::stoll(segment));
    }
    inputFile.close();

    std::vector<IntcodeComputer> computers;
    for (int address = 0; address < 50; ++address) {
        computers.emplace_back(program, address);
    }

    std::vector<std::deque<std::pair<long long, long long>>> packet_queues(50);

    std::optional<std::pair<long long, long long>> nat_packet;
    std::optional<long long> prev_nat_y;

    while (true) {
        for (int i = 0; i < 50; ++i) {
            IntcodeComputer& computer = computers[i];
            if (!packet_queues[i].empty()) {
                long long x = packet_queues[i].front().first;
                long long y = packet_queues[i].front().second;
                packet_queues[i].pop_front();
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
                    nat_packet = {x, y};
                } else if (dest >= 0 && dest < 50) {
                    packet_queues[dest].push_back({x, y});
                }
            }
        }

        bool all_queues_empty = true;
        for (int i = 0; i < 50; ++i) {
            if (!packet_queues[i].empty()) {
                all_queues_empty = false;
                break;
            }
        }

        bool all_computers_idle = true;
        for (int i = 0; i < 50; ++i) {
            if (!computers[i].idle) {
                all_computers_idle = false;
                break;
            }
        }
        
        if (all_queues_empty && all_computers_idle) {
            if (nat_packet.has_value()) {
                long long x = nat_packet->first;
                long long y = nat_packet->second;

                if (prev_nat_y.has_value() && y == prev_nat_y.value()) {
                    std::cout << y << std::endl;
                    return 0;
                }
                prev_nat_y = y;
                packet_queues[0].push_back({x, y});
            }
        }
    }

    return 0;
}
