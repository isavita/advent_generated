
#include <iostream>
#include <vector>
#include <string>
#include <map>
#include <deque>
#include <fstream>
#include <sstream>
#include <stdexcept>
#include <algorithm> // For std::all_of, std::equal
#include <numeric>   // For std::iota (not used, but generally useful)

class IntcodeComputer {
public:
    std::map<long long, long long> memory;
    long long pointer;
    long long relative_base;
    std::deque<long long> inputs;
    std::deque<long long> outputs;
    bool halted;

    IntcodeComputer(const std::vector<long long>& program)
        : pointer(0), relative_base(0), halted(false) {
        for (size_t i = 0; i < program.size(); ++i) {
            memory[i] = program[i];
        }
    }

    long long get_param_value(int mode, long long param) {
        if (mode == 0) { // Position mode
            return memory[param];
        } else if (mode == 1) { // Immediate mode
            return param;
        } else if (mode == 2) { // Relative mode
            return memory[relative_base + param];
        } else {
            throw std::runtime_error("Unknown parameter mode for reading: " + std::to_string(mode));
        }
    }

    void set_param_value(int mode, long long param, long long value) {
        if (mode == 0) { // Position mode
            memory[param] = value;
        } else if (mode == 2) { // Relative mode
            memory[relative_base + param] = value;
        } else {
            throw std::runtime_error("Unknown parameter mode for writing: " + std::to_string(mode));
        }
    }

    void run() {
        while (!halted) {
            long long instruction = memory[pointer];
            int opcode = instruction % 100;
            int mode1 = (instruction / 100) % 10;
            int mode2 = (instruction / 1000) % 10;
            int mode3 = (instruction / 10000) % 10;

            if (opcode == 1) { // Addition
                long long val1 = get_param_value(mode1, memory[pointer + 1]);
                long long val2 = get_param_value(mode2, memory[pointer + 2]);
                set_param_value(mode3, memory[pointer + 3], val1 + val2);
                pointer += 4;
            } else if (opcode == 2) { // Multiplication
                long long val1 = get_param_value(mode1, memory[pointer + 1]);
                long long val2 = get_param_value(mode2, memory[pointer + 2]);
                set_param_value(mode3, memory[pointer + 3], val1 * val2);
                pointer += 4;
            } else if (opcode == 3) { // Input
                if (inputs.empty()) {
                    return; // Wait for input
                }
                long long input_value = inputs.front();
                inputs.pop_front();
                set_param_value(mode1, memory[pointer + 1], input_value);
                pointer += 2;
            } else if (opcode == 4) { // Output
                long long output_value = get_param_value(mode1, memory[pointer + 1]);
                outputs.push_back(output_value);
                pointer += 2;
            } else if (opcode == 5) { // Jump-if-true
                long long val1 = get_param_value(mode1, memory[pointer + 1]);
                long long val2 = get_param_value(mode2, memory[pointer + 2]);
                if (val1 != 0) {
                    pointer = val2;
                } else {
                    pointer += 3;
                }
            } else if (opcode == 6) { // Jump-if-false
                long long val1 = get_param_value(mode1, memory[pointer + 1]);
                long long val2 = get_param_value(mode2, memory[pointer + 2]);
                if (val1 == 0) {
                    pointer = val2;
                } else {
                    pointer += 3;
                }
            } else if (opcode == 7) { // Less than
                long long val1 = get_param_value(mode1, memory[pointer + 1]);
                long long val2 = get_param_value(mode2, memory[pointer + 2]);
                set_param_value(mode3, memory[pointer + 3], (val1 < val2) ? 1 : 0);
                pointer += 4;
            } else if (opcode == 8) { // Equals
                long long val1 = get_param_value(mode1, memory[pointer + 1]);
                long long val2 = get_param_value(mode2, memory[pointer + 2]);
                set_param_value(mode3, memory[pointer + 3], (val1 == val2) ? 1 : 0);
                pointer += 4;
            } else if (opcode == 9) { // Adjust relative base
                long long val1 = get_param_value(mode1, memory[pointer + 1]);
                relative_base += val1;
                pointer += 2;
            } else if (opcode == 99) { // Halt
                halted = true;
                return;
            } else {
                throw std::runtime_error("Unknown opcode: " + std::to_string(opcode));
            }
        }
    }
};

std::vector<long long> read_program_from_file(const std::string& filename) {
    std::vector<long long> program;
    std::ifstream file(filename);
    if (!file.is_open()) {
        throw std::runtime_error("Could not open file: " + filename);
    }
    std::string line;
    std::getline(file, line);
    std::stringstream ss(line);
    std::string segment;
    while (std::getline(ss, segment, ',')) {
        program.push_back(std::stoll(segment));
    }
    return program;
}

std::vector<std::string> parse_ascii_map(const std::deque<long long>& output_values) {
    std::vector<std::string> grid;
    std::string current_line;
    for (long long c_val : output_values) {
        char c = static_cast<char>(c_val);
        if (c == 10) { // Newline
            if (!current_line.empty()) {
                grid.push_back(current_line);
            }
            current_line.clear();
        } else {
            current_line += c;
        }
    }
    if (!current_line.empty()) { // Add last line if not ended with newline
        grid.push_back(current_line);
    }
    return grid;
}

std::vector<std::pair<int, int>> find_intersections(const std::vector<std::string>& grid) {
    std::vector<std::pair<int, int>> intersections;
    if (grid.empty() || grid[0].empty()) return intersections;

    int rows = grid.size();
    int cols = grid[0].length();

    for (int y = 1; y < rows - 1; ++y) {
        for (int x = 1; x < cols - 1; ++x) {
            if (grid[y][x] != '#') {
                continue;
            }
            if (grid[y-1][x] == '#' && grid[y+1][x] == '#' &&
                grid[y][x-1] == '#' && grid[y][x+1] == '#') {
                intersections.push_back({x, y});
            }
        }
    }
    return intersections;
}

struct RobotPosition {
    int x, y;
    char direction;
};

RobotPosition find_robot_position(const std::vector<std::string>& grid) {
    for (int y = 0; y < grid.size(); ++y) {
        for (int x = 0; x < grid[y].length(); ++x) {
            char cell = grid[y][x];
            if (cell == '^' || cell == 'v' || cell == '<' || cell == '>' || cell == 'X') {
                return {x, y, cell};
            }
        }
    }
    throw std::runtime_error("Robot not found on the scaffold.");
}

char turn_left(char direction) {
    if (direction == '^') return '<';
    if (direction == '<') return 'v';
    if (direction == 'v') return '>';
    if (direction == '>') return '^';
    throw std::runtime_error("Unknown direction for turn_left: " + std::string(1, direction));
}

char turn_right(char direction) {
    if (direction == '^') return '>';
    if (direction == '>') return 'v';
    if (direction == 'v') return '<';
    if (direction == '<') return '^';
    throw std::runtime_error("Unknown direction for turn_right: " + std::string(1, direction));
}

std::pair<int, int> move_forward_coords(int x, int y, char direction) {
    if (direction == '^') return {x, y - 1};
    if (direction == 'v') return {x, y + 1};
    if (direction == '<') return {x - 1, y};
    if (direction == '>') return {x + 1, y};
    throw std::runtime_error("Unknown direction for move_forward: " + std::string(1, direction));
}

std::vector<std::string> get_movement_path(const std::vector<std::string>& grid, int start_x, int start_y, char start_dir) {
    int x = start_x;
    int y = start_y;
    char direction = start_dir;
    std::vector<std::string> path;
    int steps = 0;

    int rows = grid.size();
    int cols = grid[0].length();

    while (true) {
        int next_x, next_y;
        std::tie(next_x, next_y) = move_forward_coords(x, y, direction);

        if (next_y >= 0 && next_y < rows && next_x >= 0 && next_x < cols && grid[next_y][next_x] == '#') {
            x = next_x;
            y = next_y;
            steps++;
        } else {
            if (steps > 0) {
                path.push_back(std::to_string(steps));
                steps = 0;
            }

            char left_dir = turn_left(direction);
            std::tie(next_x, next_y) = move_forward_coords(x, y, left_dir);
            if (next_y >= 0 && next_y < rows && next_x >= 0 && next_x < cols && grid[next_y][next_x] == '#') {
                path.push_back("L");
                direction = left_dir;
                continue;
            }

            char right_dir = turn_right(direction);
            std::tie(next_x, next_y) = move_forward_coords(x, y, right_dir);
            if (next_y >= 0 && next_y < rows && next_x >= 0 && next_x < cols && grid[next_y][next_x] == '#') {
                path.push_back("R");
                direction = right_dir;
                continue;
            }

            break; // No where to move or turn
        }
    }
    return path;
}

struct CompressedPath {
    std::string main_routine;
    std::string function_A;
    std::string function_B;
    std::string function_C;
};

size_t get_path_segment_string_length(const std::vector<std::string>& path_segment_tokens) {
    if (path_segment_tokens.empty()) return 0;
    size_t length = 0;
    for (const auto& s : path_segment_tokens) {
        length += s.length();
    }
    length += (path_segment_tokens.size() - 1); // For commas
    return length;
}

CompressedPath compress_movement(const std::vector<std::string>& path_tokens) {
    const int max_function_char_length = 20;
    const int max_pattern_token_length = 10;

    for (int a_len = 1; a_len <= max_pattern_token_length; ++a_len) {
        if (a_len > path_tokens.size()) break;
        std::vector<std::string> a_pattern(path_tokens.begin(), path_tokens.begin() + a_len);
        if (get_path_segment_string_length(a_pattern) > max_function_char_length) continue;

        for (int b_start = a_len; b_start < path_tokens.size(); ++b_start) {
            for (int b_len = 1; b_len <= max_pattern_token_length; ++b_len) {
                if (b_start + b_len > path_tokens.size()) break;
                std::vector<std::string> b_pattern(path_tokens.begin() + b_start, path_tokens.begin() + b_start + b_len);
                if (get_path_segment_string_length(b_pattern) > max_function_char_length) continue;

                for (int c_start = b_start + b_len; c_start < path_tokens.size(); ++c_start) {
                    for (int c_len = 1; c_len <= max_pattern_token_length; ++c_len) {
                        if (c_start + c_len > path_tokens.size()) break;
                        std::vector<std::string> c_pattern(path_tokens.begin() + c_start, path_tokens.begin() + c_start + c_len);
                        if (get_path_segment_string_length(c_pattern) > max_function_char_length) continue;
                        
                        // Try to compress the original path with these patterns
                        std::vector<std::string> current_main_routine_tokens = path_tokens;
                        bool changed_in_pass = true;
                        while(changed_in_pass){
                            changed_in_pass = false;
                            std::vector<std::string> next_main_routine_tokens;
                            size_t i = 0;
                            while(i < current_main_routine_tokens.size()){
                                // Try A
                                if (i + a_pattern.size() <= current_main_routine_tokens.size() &&
                                    std::equal(current_main_routine_tokens.begin() + i, current_main_routine_tokens.begin() + i + a_pattern.size(), a_pattern.begin())) {
                                    next_main_routine_tokens.push_back("A");
                                    i += a_pattern.size();
                                    changed_in_pass = true;
                                }
                                // Try B
                                else if (i + b_pattern.size() <= current_main_routine_tokens.size() &&
                                           std::equal(current_main_routine_tokens.begin() + i, current_main_routine_tokens.begin() + i + b_pattern.size(), b_pattern.begin())) {
                                    next_main_routine_tokens.push_back("B");
                                    i += b_pattern.size();
                                    changed_in_pass = true;
                                }
                                // Try C
                                else if (i + c_pattern.size() <= current_main_routine_tokens.size() &&
                                           std::equal(current_main_routine_tokens.begin() + i, current_main_routine_tokens.begin() + i + c_pattern.size(), c_pattern.begin())) {
                                    next_main_routine_tokens.push_back("C");
                                    i += c_pattern.size();
                                    changed_in_pass = true;
                                }
                                else {
                                    next_main_routine_tokens.push_back(current_main_routine_tokens[i]);
                                    i += 1;
                                }
                            }
                            current_main_routine_tokens = next_main_routine_tokens;
                        }

                        // Check if main routine is valid (only A, B, C remaining)
                        bool all_abc_valid = true;
                        for (const auto& token : current_main_routine_tokens) {
                            if (token != "A" && token != "B" && token != "C") {
                                all_abc_valid = false;
                                break;
                            }
                        }
                        if (!all_abc_valid) continue;

                        if (get_path_segment_string_length(current_main_routine_tokens) <= max_function_char_length) {
                            // Found a valid compression
                            std::string main_str;
                            for(size_t k = 0; k < current_main_routine_tokens.size(); ++k) {
                                main_str += current_main_routine_tokens[k];
                                if (k < current_main_routine_tokens.size() - 1) main_str += ',';
                            }
                            
                            std::string func_a_str;
                            for(size_t k = 0; k < a_pattern.size(); ++k) {
                                func_a_str += a_pattern[k];
                                if (k < a_pattern.size() - 1) func_a_str += ',';
                            }

                            std::string func_b_str;
                            for(size_t k = 0; k < b_pattern.size(); ++k) {
                                func_b_str += b_pattern[k];
                                if (k < b_pattern.size() - 1) func_b_str += ',';
                            }

                            std::string func_c_str;
                            for(size_t k = 0; k < c_pattern.size(); ++k) {
                                func_c_str += c_pattern[k];
                                if (k < c_pattern.size() - 1) func_c_str += ',';
                            }
                            
                            return {main_str, func_a_str, func_b_str, func_c_str};
                        }
                    }
                }
            }
        }
    }
    throw std::runtime_error("Could not compress the path into functions A, B, C.");
}


int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::vector<long long> program_initial;
    try {
        program_initial = read_program_from_file("input.txt");
    } catch (const std::runtime_error& e) {
        std::cerr << "Error reading input: " << e.what() << std::endl;
        return 1;
    }

    // Part One
    IntcodeComputer computer_part1(program_initial);
    computer_part1.run();
    std::vector<std::string> grid = parse_ascii_map(computer_part1.outputs);

    long long alignment_sum = 0;
    std::vector<std::pair<int, int>> intersections = find_intersections(grid);
    for (const auto& p : intersections) {
        alignment_sum += (long long)p.first * p.second;
    }
    std::cout << alignment_sum << std::endl;

    // Part Two
    std::vector<long long> program_part2 = program_initial;
    program_part2[0] = 2; // Set memory address 0 to 2 for Part Two
    IntcodeComputer computer_part2(program_part2);

    RobotPosition robot_start = find_robot_position(grid);
    std::vector<std::string> movement_path_tokens = get_movement_path(grid, robot_start.x, robot_start.y, robot_start.direction);

    CompressedPath compressed_path;
    try {
        compressed_path = compress_movement(movement_path_tokens);
    } catch (const std::runtime_error& e) {
        std::cerr << "Error in compressing path: " << e.what() << std::endl;
        return 1;
    }

    std::vector<std::string> input_lines = {
        compressed_path.main_routine,
        compressed_path.function_A,
        compressed_path.function_B,
        compressed_path.function_C,
        "n" // Continuous video feed input
    };

    for (const auto& line : input_lines) {
        for (char c : line) {
            computer_part2.inputs.push_back(static_cast<long long>(c));
        }
        computer_part2.inputs.push_back(10); // Newline character (ASCII 10)
    }

    computer_part2.run();

    long long dust_collected = 0;
    if (!computer_part2.outputs.empty()) {
        dust_collected = computer_part2.outputs.back(); // The last output is the dust collected
    }
    std::cout << dust_collected << std::endl;

    return 0;
}
