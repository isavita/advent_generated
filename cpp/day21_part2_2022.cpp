
#include <fstream>
#include <iostream>
#include <map>
#include <string>
#include <utility> // For std::pair

struct Monkey;

std::map<std::string, Monkey> monkeys;

struct Monkey {
    std::string name;
    long long val;
    bool has_val;
    std::string op_str;
    std::string left_name;
    std::string right_name;
    Monkey* left_ptr;
    Monkey* right_ptr;

    Monkey() : val(0), has_val(false), left_ptr(nullptr), right_ptr(nullptr) {}

    std::pair<long long, bool> solve() {
        if (has_val) {
            return {val, true};
        }

        if (left_ptr != nullptr && right_ptr != nullptr) {
            std::pair<long long, bool> left_res = left_ptr->solve();
            std::pair<long long, bool> right_res = right_ptr->solve();

            if (left_res.second && right_res.second) {
                long long l_val = left_res.first;
                long long r_val = right_res.first;

                if (op_str == "+") {
                    return {l_val + r_val, true};
                }
                if (op_str == "-") {
                    return {l_val - r_val, true};
                }
                if (op_str == "*") {
                    return {l_val * r_val, true};
                }
                if (op_str == "/") {
                    return {l_val / r_val, true};
                }
                if (op_str == "==") {
                    return {l_val == r_val ? 0 : 1, true};
                }
            }
        }
        return {0, false};
    }

    long long expect(long long x) {
        if (name == "humn") {
            return x;
        }

        std::pair<long long, bool> left_res = left_ptr->solve();
        std::pair<long long, bool> right_res = right_ptr->solve();

        if (!left_res.second) {
            if (op_str == "+") {
                return left_ptr->expect(x - right_res.first);
            }
            if (op_str == "-") {
                return left_ptr->expect(x + right_res.first);
            }
            if (op_str == "*") {
                return left_ptr->expect(x / right_res.first);
            }
            if (op_str == "/") {
                return left_ptr->expect(x * right_res.first);
            }
            if (op_str == "==") {
                return left_ptr->expect(right_res.first);
            }
        } else if (!right_res.second) {
            if (op_str == "+") {
                return right_ptr->expect(x - left_res.first);
            }
            if (op_str == "-") {
                return right_ptr->expect(left_res.first - x);
            }
            if (op_str == "*") {
                return right_ptr->expect(x / left_res.first);
            }
            if (op_str == "/") {
                return right_ptr->expect(left_res.first / x);
            }
            if (op_str == "==") {
                return right_ptr->expect(left_res.first);
            }
        }
        throw std::runtime_error("impossible to find humn value");
    }
};

void parse() {
    std::ifstream file("input.txt");
    std::string line;

    while (std::getline(file, line)) {
        size_t colon_pos = line.find(": ");
        std::string name = line.substr(0, colon_pos);
        std::string rest = line.substr(colon_pos + 2);

        Monkey& current_monkey = monkeys[name];
        current_monkey.name = name;

        bool is_digit_only = true;
        for (char c : rest) {
            if (!isdigit(c) && c != '-') {
                is_digit_only = false;
                break;
            }
        }

        if (is_digit_only) {
            current_monkey.val = std::stoll(rest);
            current_monkey.has_val = true;
        } else {
            size_t first_space = rest.find(' ');
            size_t op_space = rest.find(' ', first_space + 1);

            current_monkey.left_name = rest.substr(0, first_space);
            current_monkey.op_str = rest.substr(first_space + 1, op_space - (first_space + 1));
            current_monkey.right_name = rest.substr(op_space + 1);
        }
    }
    file.close();

    for (auto& pair : monkeys) {
        Monkey& m = pair.second;
        if (!m.left_name.empty()) {
            m.left_ptr = &monkeys[m.left_name];
            m.right_ptr = &monkeys[m.right_name];
        }
    }
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    parse();

    monkeys["humn"].has_val = false;
    monkeys["root"].op_str = "==";

    long long result = monkeys["root"].expect(0);
    std::cout << result << std::endl;

    return 0;
}
