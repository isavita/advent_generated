
#include <algorithm>
#include <fstream>
#include <iostream>
#include <string>
#include <vector>

std::vector<std::string> split(const std::string& s, const std::string& delimiter) {
    std::vector<std::string> tokens;
    size_t lastPos = 0;
    size_t pos = s.find(delimiter, lastPos);
    while (std::string::npos != pos) {
        tokens.push_back(s.substr(lastPos, pos - lastPos));
        lastPos = pos + delimiter.length();
        pos = s.find(delimiter, lastPos);
    }
    tokens.push_back(s.substr(lastPos));
    return tokens;
}

struct Monkey {
    std::vector<long long> items;
    char op_char;
    long long op_val;
    long long div;
    std::vector<int> next;
    long long inspections;

    Monkey() : op_char(' '), op_val(0), div(0), next(2), inspections(0) {}
};

Monkey parse(const std::string& s) {
    Monkey m;
    std::vector<std::string> lines = split(s, "\n");

    std::string items_str = split(lines[1], ": ")[1];
    std::vector<std::string> item_tokens = split(items_str, ", ");
    for (const std::string& item_s : item_tokens) {
        m.items.push_back(std::stoll(item_s));
    }

    std::string op_expr = split(lines[2], "= ")[1];
    std::vector<std::string> op_parts = split(op_expr, " ");
    m.op_char = op_parts[1][0];
    if (op_parts[2] == "old") {
        m.op_val = -1;
    } else {
        m.op_val = std::stoll(op_parts[2]);
    }

    m.div = std::stoll(split(lines[3], " divisible by ")[1]);

    m.next[0] = std::stoi(split(lines[4], " throw to monkey ")[1]);
    m.next[1] = std::stoi(split(lines[5], " throw to monkey ")[1]);

    return m;
}

long long monkeyBusiness(std::vector<Monkey>& monkeys, int rounds, bool worry) {
    long long common_multiple = 1;
    for (const auto& m : monkeys) {
        common_multiple *= m.div;
    }

    for (int r = 0; r < rounds; ++r) {
        for (size_t i = 0; i < monkeys.size(); ++i) {
            Monkey& m = monkeys[i];

            std::vector<long long> current_items = std::move(m.items);
            m.items.clear();

            for (long long item : current_items) {
                m.inspections++;
                long long new_worry_level = 0;

                if (m.op_char == '+') {
                    if (m.op_val == -1) {
                        new_worry_level = item + item;
                    } else {
                        new_worry_level = item + m.op_val;
                    }
                } else {
                    if (m.op_val == -1) {
                        new_worry_level = item * item;
                    } else {
                        new_worry_level = item * m.op_val;
                    }
                }

                if (worry) {
                    new_worry_level %= common_multiple;
                } else {
                    new_worry_level /= 3;
                }

                if (new_worry_level % m.div == 0) {
                    monkeys[m.next[0]].items.push_back(new_worry_level);
                } else {
                    monkeys[m.next[1]].items.push_back(new_worry_level);
                }
            }
        }
    }

    std::vector<long long> inspections;
    for (const auto& m : monkeys) {
        inspections.push_back(m.inspections);
    }

    std::sort(inspections.rbegin(), inspections.rend());

    return inspections[0] * inspections[1];
}

std::string readAll(const std::string& path) {
    std::ifstream file(path);
    if (!file.is_open()) {
        std::cerr << "Error opening file: " << path << std::endl;
        return "";
    }
    std::string content((std::istreambuf_iterator<char>(file)),
                        std::istreambuf_iterator<char>());
    return content;
}

int main() {
    std::string s = readAll("input.txt");
    if (s.empty()) return 1;

    std::vector<Monkey> monkeys_initial;
    std::vector<std::string> monkey_blocks = split(s, "\n\n");
    for (const std::string& block : monkey_blocks) {
        monkeys_initial.push_back(parse(block));
    }

    std::vector<Monkey> monkeys_part2 = monkeys_initial;

    std::cout << monkeyBusiness(monkeys_part2, 10000, true) << std::endl;

    return 0;
}
