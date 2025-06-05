
#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <sstream>
#include <map>
#include <deque>

class VM {
public:
    std::map<long long, long long> code;
    long long ip;
    std::deque<long long> input;
    std::vector<long long> output;
    long long relative_base;

    VM(const std::string& filename);
    void run();
    void send_string(const std::string& s);

private:
    void load(const std::string& filename);
    long long get_value(long long addr);
    void set_value(long long addr, long long val);
    long long get_param_value(int param_idx, int mode);
    long long get_write_address(int param_idx, int mode);
};

VM::VM(const std::string& filename) : ip(0), relative_base(0) {
    load(filename);
}

void VM::load(const std::string& filename) {
    std::ifstream file(filename);
    std::string line;
    std::getline(file, line);
    std::stringstream ss(line);
    std::string segment;
    long long i = 0;
    while(std::getline(ss, segment, ',')) {
        code[i++] = std::stoll(segment);
    }
}

long long VM::get_value(long long addr) {
    return code[addr];
}

void VM::set_value(long long addr, long long val) {
    code[addr] = val;
}

long long VM::get_param_value(int param_idx, int mode) {
    long long value_at_ip_plus_idx = get_value(ip + param_idx);
    if (mode == 0) {
        return get_value(value_at_ip_plus_idx);
    } else if (mode == 1) {
        return value_at_ip_plus_idx;
    } else if (mode == 2) {
        return get_value(relative_base + value_at_ip_plus_idx);
    }
    return 0; // Should not happen with valid modes
}

long long VM::get_write_address(int param_idx, int mode) {
    long long value_at_ip_plus_idx = get_value(ip + param_idx);
    if (mode == 0) {
        return value_at_ip_plus_idx;
    } else if (mode == 2) {
        return relative_base + value_at_ip_plus_idx;
    }
    return 0; // Immediate mode (1) is not valid for write addresses
}

void VM::run() {
    while (true) {
        long long current_instruction = get_value(ip);
        long long opcode = current_instruction % 100;
        int mode1 = (current_instruction / 100) % 10;
        int mode2 = (current_instruction / 1000) % 10;
        int mode3 = (current_instruction / 10000) % 10;

        if (opcode == 99) {
            break;
        } else if (opcode == 1) {
            set_value(get_write_address(3, mode3), get_param_value(1, mode1) + get_param_value(2, mode2));
            ip += 4;
        } else if (opcode == 2) {
            set_value(get_write_address(3, mode3), get_param_value(1, mode1) * get_param_value(2, mode2));
            ip += 4;
        } else if (opcode == 3) {
            set_value(get_write_address(1, mode1), input.front());
            input.pop_front();
            ip += 2;
        } else if (opcode == 4) {
            output.push_back(get_param_value(1, mode1));
            ip += 2;
        } else if (opcode == 5) {
            if (get_param_value(1, mode1) != 0) {
                ip = get_param_value(2, mode2);
            } else {
                ip += 3;
            }
        } else if (opcode == 6) {
            if (get_param_value(1, mode1) == 0) {
                ip = get_param_value(2, mode2);
            } else {
                ip += 3;
            }
        } else if (opcode == 7) {
            set_value(get_write_address(3, mode3), (get_param_value(1, mode1) < get_param_value(2, mode2)) ? 1 : 0);
            ip += 4;
        } else if (opcode == 8) {
            set_value(get_write_address(3, mode3), (get_param_value(1, mode1) == get_param_value(2, mode2)) ? 1 : 0);
            ip += 4;
        } else if (opcode == 9) {
            relative_base += get_param_value(1, mode1);
            ip += 2;
        }
    }
}

void VM::send_string(const std::string& s) {
    for (char c : s) {
        input.push_back(static_cast<long long>(c));
    }
    input.push_back(10LL);
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    VM vm("input.txt");

    std::vector<std::string> instructions = {
        "NOT A J",
        "NOT B T",
        "OR T J",
        "NOT C T",
        "OR T J",
        "AND D J",
        "NOT A T",
        "AND A T",
        "OR E T",
        "OR H T",
        "AND T J",
        "RUN",
    };

    for (const std::string& instr : instructions) {
        vm.send_string(instr);
    }

    vm.run();

    for (long long output_val : vm.output) {
        if (output_val >= 0 && output_val <= 127) {
            std::cout << static_cast<char>(output_val);
        } else {
            std::cout << output_val << std::endl;
        }
    }

    return 0;
}
