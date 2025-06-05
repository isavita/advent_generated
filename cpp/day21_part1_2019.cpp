
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
    std::deque<long long> output;
    long long relative_base;

    VM(const std::string& filename);
    void run();

private:
    long long getValue(long long addr);
    void setValue(long long addr, long long val);
    long long getParam(int index, const std::vector<int>& modes);
    long long getAddress(int index, const std::vector<int>& modes);
};

VM::VM(const std::string& filename) : ip(0), relative_base(0) {
    std::ifstream file(filename);
    std::string line;
    std::getline(file, line);
    std::stringstream ss(line);
    std::string segment;
    long long i = 0;
    while (std::getline(ss, segment, ',')) {
        code[i++] = std::stoll(segment);
    }
}

long long VM::getValue(long long addr) {
    return code[addr];
}

void VM::setValue(long long addr, long long val) {
    code[addr] = val;
}

long long VM::getParam(int index, const std::vector<int>& modes) {
    int mode = modes[index - 1];
    long long immediate_value = getValue(ip + index);

    if (mode == 0) {
        return getValue(immediate_value);
    } else if (mode == 1) {
        return immediate_value;
    } else if (mode == 2) {
        return getValue(relative_base + immediate_value);
    }
    return 0;
}

long long VM::getAddress(int index, const std::vector<int>& modes) {
    int mode = modes[index - 1];
    long long immediate_value = getValue(ip + index);

    if (mode == 0) {
        return immediate_value;
    } else if (mode == 2) {
        return relative_base + immediate_value;
    }
    return 0;
}

void VM::run() {
    while (true) {
        long long current_instruction = getValue(ip);
        int opcode = current_instruction % 100;
        std::vector<int> modes = {
            static_cast<int>((current_instruction / 100) % 10),
            static_cast<int>((current_instruction / 1000) % 10),
            static_cast<int>((current_instruction / 10000) % 10)
        };

        if (opcode == 1) {
            long long val1 = getParam(1, modes);
            long long val2 = getParam(2, modes);
            long long addr = getAddress(3, modes);
            setValue(addr, val1 + val2);
            ip += 4;
        } else if (opcode == 2) {
            long long val1 = getParam(1, modes);
            long long val2 = getParam(2, modes);
            long long addr = getAddress(3, modes);
            setValue(addr, val1 * val2);
            ip += 4;
        } else if (opcode == 3) {
            long long addr = getAddress(1, modes);
            setValue(addr, input.front());
            input.pop_front();
            ip += 2;
        } else if (opcode == 4) {
            output.push_back(getParam(1, modes));
            ip += 2;
        } else if (opcode == 5) {
            if (getParam(1, modes) != 0) {
                ip = getParam(2, modes);
            } else {
                ip += 3;
            }
        } else if (opcode == 6) {
            if (getParam(1, modes) == 0) {
                ip = getParam(2, modes);
            } else {
                ip += 3;
            }
        } else if (opcode == 7) {
            long long val1 = getParam(1, modes);
            long long val2 = getParam(2, modes);
            long long addr = getAddress(3, modes);
            setValue(addr, (val1 < val2) ? 1 : 0);
            ip += 4;
        } else if (opcode == 8) {
            long long val1 = getParam(1, modes);
            long long val2 = getParam(2, modes);
            long long addr = getAddress(3, modes);
            setValue(addr, (val1 == val2) ? 1 : 0);
            ip += 4;
        } else if (opcode == 9) {
            relative_base += getParam(1, modes);
            ip += 2;
        } else if (opcode == 99) {
            break;
        } else {
            throw std::runtime_error("Unknown opcode: " + std::to_string(opcode) + " at ip " + std::to_string(ip));
        }
    }
}

void sendString(VM& vm, const std::string& s) {
    for (char c : s) {
        vm.input.push_back(static_cast<long long>(c));
    }
    vm.input.push_back(static_cast<long long>('\n'));
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
        "WALK"
    };

    for (const auto& instr : instructions) {
        sendString(vm, instr);
    }

    vm.run();

    for (long long output_val : vm.output) {
        if (output_val > 127) {
            std::cout << output_val << std::endl;
            break;
        }
    }

    return 0;
}
