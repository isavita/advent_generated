
#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <sstream>
#include <algorithm>
#include <array>
#include <numeric>

struct Operation {
    char type;
    long long value;
    bool use_old;

    long long apply(long long old_worry) const {
        long long operand_val = use_old ? old_worry : value;
        if (type == '+') {
            return old_worry + operand_val;
        }
        return old_worry * operand_val;
    }
};

struct Monkey {
    std::vector<long long> items;
    Operation operation;
    long long div_test;
    std::array<int, 2> next_monkeys;
    long long inspections;

    Monkey() : inspections(0) {}
};

std::string read_all(const std::string& path) {
    std::ifstream file(path);
    std::stringstream buffer;
    buffer << file.rdbuf();
    return buffer.str();
}

Monkey parse(const std::string& s) {
    Monkey m;
    std::stringstream ss(s);
    std::string line;

    std::getline(ss, line); 
    std::getline(ss, line); 
    size_t start_items_pos = line.find(": ") + 2;
    std::string items_str = line.substr(start_items_pos);
    std::stringstream items_ss(items_str);
    std::string item_str;
    while (std::getline(items_ss, item_str, ',')) {
        m.items.push_back(std::stoll(item_str));
    }

    std::getline(ss, line);
    size_t eq_pos = line.find("= ") + 2;
    std::string op_expr = line.substr(eq_pos);
    std::stringstream op_ss(op_expr);
    std::string old_str, op_type_str, operand_str;
    op_ss >> old_str >> op_type_str >> operand_str;

    m.operation.type = op_type_str[0];
    if (operand_str == "old") {
        m.operation.use_old = true;
    } else {
        m.operation.use_old = false;
        m.operation.value = std::stoll(operand_str);
    }

    std::getline(ss, line);
    size_t div_pos = line.find(" divisible by ") + std::string(" divisible by ").length();
    m.div_test = std::stoll(line.substr(div_pos));

    std::getline(ss, line);
    size_t true_pos = line.find(" throw to monkey ") + std::string(" throw to monkey ").length();
    m.next_monkeys[0] = std::stoi(line.substr(true_pos));

    std::getline(ss, line);
    size_t false_pos = line.find(" throw to monkey ") + std::string(" throw to monkey ").length();
    m.next_monkeys[1] = std::stoi(line.substr(false_pos));

    return m;
}

long long monkey_business(std::vector<Monkey>& monkeys, int rounds, bool worry) {
    long long common_multiple = 1;
    if (worry) {
        for (const auto& m : monkeys) {
            common_multiple *= m.div_test;
        }
    }

    for (int r = 0; r < rounds; ++r) {
        for (int i = 0; i < monkeys.size(); ++i) {
            Monkey& current_monkey = monkeys[i];
            
            std::vector<long long> items_to_process;
            items_to_process.swap(current_monkey.items);

            for (long long item_worry : items_to_process) {
                current_monkey.inspections++;
                
                long long new_worry = current_monkey.operation.apply(item_worry);

                if (worry) {
                    new_worry %= common_multiple;
                } else {
                    new_worry /= 3;
                }

                int target_monkey_idx = current_monkey.next_monkeys[new_worry % current_monkey.div_test == 0 ? 0 : 1];
                
                monkeys[target_monkey_idx].items.push_back(new_worry);
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

int main() {
    std::vector<Monkey> monkeys;
    std::string s = read_all("input.txt");

    size_t start = 0;
    size_t end = s.find("\n\n");
    while (end != std::string::npos) {
        monkeys.push_back(parse(s.substr(start, end - start)));
        start = end + 2;
        end = s.find("\n\n", start);
    }
    monkeys.push_back(parse(s.substr(start)));

    std::cout << monkey_business(monkeys, 20, false) << std::endl;

    return 0;
}
