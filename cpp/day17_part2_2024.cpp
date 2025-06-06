
#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <sstream>
#include <cstdint>
#include <stack>
#include <map>
#include <utility>

struct Program {
    int64_t a;
    int64_t b;
    int64_t c;
    std::vector<int> program;
};

int64_t computeOperand(int val, int64_t a, int64_t b, int64_t c) {
    switch (val) {
        case 0: case 1: case 2: case 3: return val;
        case 4: return a;
        case 5: return b;
        case 6: return c;
        default: return 0;
    }
}

std::vector<int> simulateComputer(const Program& p) {
    std::vector<int> outs;
    int64_t a = p.a, b = p.b, c = p.c;
    const std::vector<int>& input = p.program;

    for (long long i = 1; i < (long long)input.size(); i += 2) {
        int cmd = input[i - 1];
        int operand_val = input[i];

        switch (cmd) {
            case 0: a >>= computeOperand(operand_val, a, b, c); break;
            case 1: b ^= static_cast<int64_t>(operand_val); break;
            case 2: b = computeOperand(operand_val, a, b, c) % 8; break;
            case 3:
                if (a != 0) {
                    i = operand_val - 1;
                }
                break;
            case 4: b ^= c; break;
            case 5: outs.push_back(computeOperand(operand_val, a, b, c) % 8); break;
            case 6: b = a >> computeOperand(operand_val, a, b, c); break;
            case 7: c = a >> computeOperand(operand_val, a, b, c); break;
        }
    }
    return outs;
}

int64_t find_min_value(const Program& p) {
    using State = std::pair<int, int64_t>;
    std::stack<State> dfs_stack;
    std::map<State, bool> seen;
    const auto& program_code = p.program;
    size_t target_depth = program_code.size();

    dfs_stack.push({0, 0});

    while (!dfs_stack.empty()) {
        State current_state = dfs_stack.top();
        dfs_stack.pop();

        if (seen.count(current_state)) {
            continue;
        }
        seen[current_state] = true;

        int depth = current_state.first;
        int64_t score = current_state.second;

        if (depth == target_depth) {
            return score;
        }

        for (int i = 7; i >= 0; --i) {
            int64_t new_score = static_cast<int64_t>(i) + 8 * score;
            Program test_program = {new_score, p.b, p.c, program_code};
            std::vector<int> result = simulateComputer(test_program);

            if (!result.empty() && result[0] == program_code[program_code.size() - 1 - depth]) {
                State next_state = {depth + 1, new_score};
                if (!seen.count(next_state)) {
                    dfs_stack.push(next_state);
                }
            }
        }
    }
    return -1;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(nullptr);

    std::ifstream file("input.txt");
    Program p{};
    std::string line;

    while (std::getline(file, line)) {
        if (line.rfind("Register A:", 0) == 0) {
            p.a = std::stoll(line.substr(line.find(':') + 1));
        } else if (line.rfind("Register B:", 0) == 0) {
            p.b = std::stoll(line.substr(line.find(':') + 1));
        } else if (line.rfind("Register C:", 0) == 0) {
            p.c = std::stoll(line.substr(line.find(':') + 1));
        } else if (line.rfind("Program:", 0) == 0) {
            std::stringstream ss(line.substr(line.find(':') + 1));
            std::string num_str;
            while (std::getline(ss, num_str, ',')) {
                p.program.push_back(std::stoi(num_str));
            }
        }
    }

    int64_t min_val = find_min_value(p);
    std::cout << min_val << std::endl;

    return 0;
}
