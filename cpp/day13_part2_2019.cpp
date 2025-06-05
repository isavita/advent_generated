
#include <iostream>
#include <vector>
#include <string>
#include <sstream>
#include <unordered_map>
#include <fstream>
#include <deque>

class IntcodeComputer {
public:
    std::unordered_map<long long, long long> memory;
    long long ip;
    long long relative_base;
    std::deque<long long> input_queue;
    std::deque<long long> output_queue;
    bool halted;
    bool waiting_for_input;

    IntcodeComputer(const std::vector<long long>& program) {
        for (size_t i = 0; i < program.size(); ++i) {
            memory[i] = program[i];
        }
        ip = 0;
        relative_base = 0;
        halted = false;
        waiting_for_input = false;
    }

    void addInput(long long val) {
        input_queue.push_back(val);
        waiting_for_input = false;
    }

    void run() {
        if (halted) return;

        while (true) {
            long long instruction = memory[ip];
            long long opcode = instruction % 100;
            long long modes[3];
            modes[0] = (instruction / 100) % 10;
            modes[1] = (instruction / 1000) % 10;
            modes[2] = (instruction / 10000) % 10;

            if (opcode == 99) {
                halted = true;
                break;
            }

            auto get_param = [&](int offset, int mode) {
                long long val_at_offset = memory[ip + offset];
                if (mode == 0) return memory[val_at_offset];
                if (mode == 1) return val_at_offset;
                if (mode == 2) return memory[relative_base + val_at_offset];
                return 0LL;
            };

            auto get_write_addr = [&](int offset, int mode) {
                long long val_at_offset = memory[ip + offset];
                if (mode == 0) return val_at_offset;
                if (mode == 2) return relative_base + val_at_offset;
                return 0LL;
            };

            switch (opcode) {
                case 1: {
                    long long val1 = get_param(1, modes[0]);
                    long long val2 = get_param(2, modes[1]);
                    long long addr3 = get_write_addr(3, modes[2]);
                    memory[addr3] = val1 + val2;
                    ip += 4;
                    break;
                }
                case 2: {
                    long long val1 = get_param(1, modes[0]);
                    long long val2 = get_param(2, modes[1]);
                    long long addr3 = get_write_addr(3, modes[2]);
                    memory[addr3] = val1 * val2;
                    ip += 4;
                    break;
                }
                case 3: {
                    if (input_queue.empty()) {
                        waiting_for_input = true;
                        return;
                    }
                    long long addr1 = get_write_addr(1, modes[0]);
                    memory[addr1] = input_queue.front();
                    input_queue.pop_front();
                    ip += 2;
                    break;
                }
                case 4: {
                    long long val1 = get_param(1, modes[0]);
                    output_queue.push_back(val1);
                    ip += 2;
                    return;
                }
                case 5: {
                    long long val1 = get_param(1, modes[0]);
                    long long val2 = get_param(2, modes[1]);
                    if (val1 != 0) {
                        ip = val2;
                    } else {
                        ip += 3;
                    }
                    break;
                }
                case 6: {
                    long long val1 = get_param(1, modes[0]);
                    long long val2 = get_param(2, modes[1]);
                    if (val1 == 0) {
                        ip = val2;
                    } else {
                        ip += 3;
                    }
                    break;
                }
                case 7: {
                    long long val1 = get_param(1, modes[0]);
                    long long val2 = get_param(2, modes[1]);
                    long long addr3 = get_write_addr(3, modes[2]);
                    memory[addr3] = (val1 < val2) ? 1 : 0;
                    ip += 4;
                    break;
                }
                case 8: {
                    long long val1 = get_param(1, modes[0]);
                    long long val2 = get_param(2, modes[1]);
                    long long addr3 = get_write_addr(3, modes[2]);
                    memory[addr3] = (val1 == val2) ? 1 : 0;
                    ip += 4;
                    break;
                }
                case 9: {
                    long long val1 = get_param(1, modes[0]);
                    relative_base += val1;
                    ip += 2;
                    break;
                }
                default:
                    halted = true;
                    break;
            }
        }
    }

    bool hasOutput() const {
        return !output_queue.empty();
    }

    long long getOutput() {
        long long val = output_queue.front();
        output_queue.pop_front();
        return val;
    }

    bool isHalted() const {
        return halted;
    }

    bool isWaitingForInput() const {
        return waiting_for_input;
    }
};

std::vector<long long> parse_input(const std::string& file_path) {
    std::vector<long long> program;
    std::ifstream file(file_path);
    std::string line;
    if (std::getline(file, line)) {
        std::stringstream ss(line);
        std::string segment;
        while (std::getline(ss, segment, ',')) {
            program.push_back(std::stoll(segment));
        }
    }
    return program;
}

long long play_game(std::vector<long long> program_initial) {
    IntcodeComputer computer(program_initial);
    computer.memory[0] = 2;

    long long score = 0;
    long long ball_x = 0;
    long long paddle_x = 0;

    while (!computer.isHalted()) {
        computer.run();

        if (computer.isHalted()) {
            break;
        }

        if (computer.isWaitingForInput()) {
            long long input_val = 0;
            if (ball_x > paddle_x) {
                input_val = 1;
            } else if (ball_x < paddle_x) {
                input_val = -1;
            }
            computer.addInput(input_val);
            continue;
        }

        if (computer.hasOutput()) {
            long long x = computer.getOutput();
            computer.run();
            long long y = computer.getOutput();
            computer.run();
            long long tile_id = computer.getOutput();

            if (x == -1 && y == 0) {
                score = tile_id;
            } else {
                if (tile_id == 3) {
                    paddle_x = x;
                } else if (tile_id == 4) {
                    ball_x = x;
                }
            }
        }
    }
    return score;
}

int main() {
    std::vector<long long> program = parse_input("input.txt");
    long long final_score = play_game(program);
    std::cout << final_score << std::endl;
    return 0;
}
