
#include <iostream>
#include <vector>
#include <map>
#include <string>
#include <fstream>
#include <sstream>
#include <stdexcept>

long long get_mem(const std::map<long long, long long>& memory, long long address) {
    auto it = memory.find(address);
    if (it == memory.end()) {
        return 0;
    }
    return it->second;
}

long long get_parameter_value(const std::map<long long, long long>& memory, long long ip, long long relative_base, int param_index) {
    long long instruction = get_mem(memory, ip);
    int mode = (instruction / (param_index == 1 ? 100 : (param_index == 2 ? 1000 : 10000))) % 10;
    long long param = get_mem(memory, ip + param_index);

    switch (mode) {
        case 0: // Position mode
            return get_mem(memory, param);
        case 1: // Immediate mode
            return param;
        case 2: // Relative mode
            return get_mem(memory, relative_base + param);
        default:
            exit(1); // Unknown mode
    }
}

long long get_parameter_address(const std::map<long long, long long>& memory, long long ip, long long relative_base, int param_index) {
    long long instruction = get_mem(memory, ip);
    int mode = (instruction / (param_index == 1 ? 100 : (param_index == 2 ? 1000 : 10000))) % 10;
    long long param = get_mem(memory, ip + param_index);

    switch (mode) {
        case 0: // Position mode
            return param;
        case 2: // Relative mode
            return relative_base + param;
        default:
             exit(1); // Invalid write mode
    }
}


long long run_intcode(std::map<long long, long long>& memory, long long input_val) {
    long long ip = 0;
    long long relative_base = 0;
    long long output = 0;

    while (true) {
        long long instruction = get_mem(memory, ip);
        int opcode = instruction % 100;

        switch (opcode) {
            case 1: {
                long long val1 = get_parameter_value(memory, ip, relative_base, 1);
                long long val2 = get_parameter_value(memory, ip, relative_base, 2);
                long long dest_addr = get_parameter_address(memory, ip, relative_base, 3);
                memory[dest_addr] = val1 + val2;
                ip += 4;
                break;
            }
            case 2: {
                long long val1 = get_parameter_value(memory, ip, relative_base, 1);
                long long val2 = get_parameter_value(memory, ip, relative_base, 2);
                long long dest_addr = get_parameter_address(memory, ip, relative_base, 3);
                memory[dest_addr] = val1 * val2;
                ip += 4;
                break;
            }
            case 3: {
                long long dest_addr = get_parameter_address(memory, ip, relative_base, 1);
                memory[dest_addr] = input_val;
                ip += 2;
                break;
            }
            case 4: {
                output = get_parameter_value(memory, ip, relative_base, 1);
                ip += 2;
                break;
            }
            case 5: {
                long long val1 = get_parameter_value(memory, ip, relative_base, 1);
                long long val2 = get_parameter_value(memory, ip, relative_base, 2);
                if (val1 != 0) {
                    ip = val2;
                } else {
                    ip += 3;
                }
                break;
            }
            case 6: {
                 long long val1 = get_parameter_value(memory, ip, relative_base, 1);
                long long val2 = get_parameter_value(memory, ip, relative_base, 2);
                if (val1 == 0) {
                    ip = val2;
                } else {
                    ip += 3;
                }
                break;
            }
            case 7: {
                long long val1 = get_parameter_value(memory, ip, relative_base, 1);
                long long val2 = get_parameter_value(memory, ip, relative_base, 2);
                long long dest_addr = get_parameter_address(memory, ip, relative_base, 3);
                memory[dest_addr] = (val1 < val2) ? 1 : 0;
                ip += 4;
                break;
            }
            case 8: {
                long long val1 = get_parameter_value(memory, ip, relative_base, 1);
                long long val2 = get_parameter_value(memory, ip, relative_base, 2);
                long long dest_addr = get_parameter_address(memory, ip, relative_base, 3);
                memory[dest_addr] = (val1 == val2) ? 1 : 0;
                ip += 4;
                break;
            }
             case 9: {
                long long val1 = get_parameter_value(memory, ip, relative_base, 1);
                relative_base += val1;
                ip += 2;
                break;
            }
            case 99:
                return output;
            default:
                exit(1); // Unknown opcode
        }
    }
}

int main() {
    std::map<long long, long long> initial_memory;
    std::ifstream file("input.txt");
    std::string line;

    if (std::getline(file, line)) {
        std::stringstream ss(line);
        std::string segment;
        long long i = 0;
        while (std::getline(ss, segment, ',')) {
            try {
                initial_memory[i++] = std::stoll(segment);
            } catch (...) {
                 return 1; // Error during parsing
            }
        }
    } else {
         return 1; // Error reading file
    }
    file.close();

    long long result = run_intcode(initial_memory, 2);
    std::cout << result << std::endl;

    return 0;
}
