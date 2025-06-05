
#include <iostream>
#include <vector>
#include <string>
#include <sstream>
#include <map>
#include <deque>
#include <set>
#include <utility> // For std::pair
#include <fstream>
#include <stdexcept> // For std::runtime_error, std::invalid_argument

class IntcodeComputer {
public:
    std::map<long long, long long> memory;
    long long ip;
    long long relative_base;
    bool halted;
    std::deque<long long> input_queue;
    std::deque<long long> output_queue;

    IntcodeComputer(const std::vector<long long>& program) : ip(0), relative_base(0), halted(false) {
        for (size_t i = 0; i < program.size(); ++i) {
            memory[i] = program[i];
        }
    }

    long long get_parameter(int mode, int offset) {
        long long param_val_at_offset = memory[ip + offset];
        if (mode == 0) { // Position mode
            return memory[param_val_at_offset];
        } else if (mode == 1) { // Immediate mode
            return param_val_at_offset;
        } else if (mode == 2) { // Relative mode
            return memory[relative_base + param_val_at_offset];
        } else {
            throw std::runtime_error("Unknown parameter mode for reading: " + std::to_string(mode));
        }
    }

    void set_parameter(int mode, int offset, long long value) {
        long long target_addr = memory[ip + offset];
        if (mode == 0) { // Position mode
            memory[target_addr] = value;
        } else if (mode == 2) { // Relative mode
            memory[relative_base + target_addr] = value;
        } else {
            throw std::runtime_error("Unknown parameter mode for writing: " + std::to_string(mode));
        }
    }

    void run() {
        while (!halted) {
            long long instruction = memory[ip];
            int opcode = instruction % 100;
            int modes[3] = {
                static_cast<int>((instruction / 100) % 10),
                static_cast<int>((instruction / 1000) % 10),
                static_cast<int>((instruction / 10000) % 10)
            };

            if (opcode == 1) { // Addition
                long long param1 = get_parameter(modes[0], 1);
                long long param2 = get_parameter(modes[1], 2);
                set_parameter(modes[2], 3, param1 + param2);
                ip += 4;
            } else if (opcode == 2) { // Multiplication
                long long param1 = get_parameter(modes[0], 1);
                long long param2 = get_parameter(modes[1], 2);
                set_parameter(modes[2], 3, param1 * param2);
                ip += 4;
            } else if (opcode == 3) { // Input
                if (input_queue.empty()) {
                    return;
                }
                long long input_val = input_queue.front();
                input_queue.pop_front();
                set_parameter(modes[0], 1, input_val);
                ip += 2;
            } else if (opcode == 4) { // Output
                long long output_val = get_parameter(modes[0], 1);
                output_queue.push_back(output_val);
                ip += 2;
                return;
            } else if (opcode == 5) { // Jump-if-true
                long long param1 = get_parameter(modes[0], 1);
                long long param2 = get_parameter(modes[1], 2);
                if (param1 != 0) {
                    ip = param2;
                } else {
                    ip += 3;
                }
            } else if (opcode == 6) { // Jump-if-false
                long long param1 = get_parameter(modes[0], 1);
                long long param2 = get_parameter(modes[1], 2);
                if (param1 == 0) {
                    ip = param2;
                } else {
                    ip += 3;
                }
            } else if (opcode == 7) { // Less than
                long long param1 = get_parameter(modes[0], 1);
                long long param2 = get_parameter(modes[1], 2);
                set_parameter(modes[2], 3, (param1 < param2) ? 1 : 0);
                ip += 4;
            } else if (opcode == 8) { // Equals
                long long param1 = get_parameter(modes[0], 1);
                long long param2 = get_parameter(modes[1], 2);
                set_parameter(modes[2], 3, (param1 == param2) ? 1 : 0);
                ip += 4;
            } else if (opcode == 9) { // Adjust relative base
                long long param1 = get_parameter(modes[0], 1);
                relative_base += param1;
                ip += 2;
            } else if (opcode == 99) { // Halt
                halted = true;
                break;
            } else {
                throw std::runtime_error("Unknown opcode: " + std::to_string(opcode));
            }
        }
    }
};

class Droid {
public:
    IntcodeComputer computer;
    std::map<int, std::pair<int, int>> direction_map;
    std::pair<int, int> current_position;
    std::map<std::pair<int, int>, int> grid;
    std::pair<int, int> oxygen_position;
    bool oxygen_found;

    Droid(const std::vector<long long>& program) : computer(program), current_position({0, 0}), oxygen_found(false) {
        direction_map[1] = {0, -1};
        direction_map[2] = {0, 1};
        direction_map[3] = {-1, 0};
        direction_map[4] = {1, 0};
        grid[current_position] = 1;
    }

    int send_move_command(int direction) {
        computer.input_queue.push_back(direction);
        while (computer.output_queue.empty() && !computer.halted) {
            computer.run();
        }
        if (computer.halted && computer.output_queue.empty()) {
            throw std::runtime_error("Intcode program halted unexpectedly before producing output.");
        }
        if (computer.output_queue.empty()) {
             throw std::runtime_error("Intcode program is waiting for input or in an unexpected state, but no output.");
        }
        long long status = computer.output_queue.front();
        computer.output_queue.pop_front();
        return static_cast<int>(status);
    }

    int get_opposite_direction(int direction) {
        if (direction == 1) return 2;
        if (direction == 2) return 1;
        if (direction == 3) return 4;
        if (direction == 4) return 3;
        throw std::invalid_argument("Invalid direction.");
    }

    std::vector<int> find_path(std::pair<int, int> start, std::pair<int, int> end) {
        std::deque<std::pair<std::pair<int, int>, std::vector<int>>> q;
        q.push_back({start, {}});
        std::set<std::pair<int, int>> visited;
        visited.insert(start);

        while (!q.empty()) {
            auto [pos, path] = q.front();
            q.pop_front();

            if (pos == end) {
                return path;
            }

            for (int direction : {1, 2, 3, 4}) {
                int dx = direction_map[direction].first;
                int dy = direction_map[direction].second;
                std::pair<int, int> new_pos = {pos.first + dx, pos.second + dy};

                if (visited.count(new_pos)) {
                    continue;
                }
                
                auto it = grid.find(new_pos);
                if (it == grid.end() || it->second == 0) {
                    continue;
                }

                visited.insert(new_pos);
                std::vector<int> new_path = path;
                new_path.push_back(direction);
                q.push_back({new_pos, new_path});
            }
        }
        throw std::runtime_error("No path found from start to end.");
    }

    void move_to(std::pair<int, int> target) {
        if (current_position == target) {
            return;
        }
        std::vector<int> path = find_path(current_position, target);
        for (int direction : path) {
            int status = send_move_command(direction);
            if (status == 0) {
                throw std::runtime_error("Unexpected wall encountered while moving to target position.");
            }
            int dx = direction_map[direction].first;
            int dy = direction_map[direction].second;
            current_position = {current_position.first + dx, current_position.second + dy};
            if (status == 2) {
                oxygen_position = current_position;
                oxygen_found = true;
            }
        }
    }

    int explore() {
        std::deque<std::pair<std::pair<int, int>, int>> q;
        q.push_back({current_position, 0});
        std::set<std::pair<int, int>> visited_explore;
        visited_explore.insert(current_position);

        while (!q.empty()) {
            auto [pos, steps] = q.front();
            q.pop_front();

            move_to(pos);

            for (int direction : {1, 2, 3, 4}) {
                int dx = direction_map[direction].first;
                int dy = direction_map[direction].second;
                std::pair<int, int> new_pos = {pos.first + dx, pos.second + dy};

                if (visited_explore.count(new_pos)) {
                    continue;
                }

                int status = send_move_command(direction);
                if (status == 0) {
                    grid[new_pos] = 0;
                } else {
                    grid[new_pos] = (status == 1) ? 1 : 2;
                    visited_explore.insert(new_pos);
                    q.push_back({new_pos, steps + 1});

                    if (status == 2) {
                        oxygen_position = new_pos;
                        oxygen_found = true;
                        return steps + 1;
                    }

                    int opposite_direction = get_opposite_direction(direction);
                    send_move_command(opposite_direction);
                }
            }
        }
        return -1;
    }
};

std::vector<long long> parse_input(const std::string& file_path) {
    std::vector<long long> program;
    std::ifstream file(file_path);
    if (!file.is_open()) {
        throw std::runtime_error("Could not open file: " + file_path);
    }
    std::string segment;
    std::string line;
    std::getline(file, line);
    std::stringstream ss(line);

    while (std::getline(ss, segment, ',')) {
        program.push_back(std::stoll(segment));
    }
    return program;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    try {
        std::string input_file = "input.txt";
        std::vector<long long> program = parse_input(input_file);
        
        Droid droid(program);
        int steps = droid.explore();
        
        std::cout << steps << std::endl;
        
    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << std::endl;
        return 1;
    }

    return 0;
}

