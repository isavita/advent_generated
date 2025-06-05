
#include <iostream>
#include <vector>
#include <string>
#include <sstream>
#include <fstream>
#include <queue>
#include <thread>
#include <algorithm>
#include <cmath>
#include <chrono>

class VM {
public:
    std::vector<long long> code;
    long long ip;
    std::queue<long long>& input_queue;
    std::queue<long long>& output_queue;

    VM(std::vector<long long> initial_code, std::queue<long long>& in_q, std::queue<long long>& out_q)
        : code(std::move(initial_code)), ip(0), input_queue(in_q), output_queue(out_q) {}

    void run() {
        while (true) {
            long long cmd = code[ip];
            long long opcode = cmd % 100;

            long long p1_val, p2_val;
            long long target_addr;

            switch (opcode) {
                case 1: // add
                    p1_val = get_param(ip + 1, is_immediate(cmd, 1));
                    p2_val = get_param(ip + 2, is_immediate(cmd, 2));
                    target_addr = code[ip + 3];
                    code[target_addr] = p1_val + p2_val;
                    ip += 4;
                    break;
                case 2: // multiply
                    p1_val = get_param(ip + 1, is_immediate(cmd, 1));
                    p2_val = get_param(ip + 2, is_immediate(cmd, 2));
                    target_addr = code[ip + 3];
                    code[target_addr] = p1_val * p2_val;
                    ip += 4;
                    break;
                case 3: // read
                    target_addr = code[ip + 1];
                    while (input_queue.empty()) {
                        std::this_thread::sleep_for(std::chrono::milliseconds(1));
                    }
                    code[target_addr] = input_queue.front();
                    input_queue.pop();
                    ip += 2;
                    break;
                case 4: // write
                    p1_val = get_param(ip + 1, is_immediate(cmd, 1));
                    output_queue.push(p1_val);
                    ip += 2;
                    break;
                case 5: // jump not zero
                    p1_val = get_param(ip + 1, is_immediate(cmd, 1));
                    p2_val = get_param(ip + 2, is_immediate(cmd, 2));
                    if (p1_val != 0) {
                        ip = p2_val;
                    } else {
                        ip += 3;
                    }
                    break;
                case 6: // jump zero
                    p1_val = get_param(ip + 1, is_immediate(cmd, 1));
                    p2_val = get_param(ip + 2, is_immediate(cmd, 2));
                    if (p1_val == 0) {
                        ip = p2_val;
                    } else {
                        ip += 3;
                    }
                    break;
                case 7: // less than
                    p1_val = get_param(ip + 1, is_immediate(cmd, 1));
                    p2_val = get_param(ip + 2, is_immediate(cmd, 2));
                    target_addr = code[ip + 3];
                    code[target_addr] = (p1_val < p2_val) ? 1 : 0;
                    ip += 4;
                    break;
                case 8: // equal
                    p1_val = get_param(ip + 1, is_immediate(cmd, 1));
                    p2_val = get_param(ip + 2, is_immediate(cmd, 2));
                    target_addr = code[ip + 3];
                    code[target_addr] = (p1_val == p2_val) ? 1 : 0;
                    ip += 4;
                    break;
                case 99: // halt
                    return;
                default:
                    return;
            }
        }
    }

private:
    long long get_param(long long address_idx, bool immediate) {
        return immediate ? code[address_idx] : code[code[address_idx]];
    }

    bool is_immediate(long long cmd, int param_num) {
        long long divisor = 1;
        for (int i = 0; i < param_num + 1; ++i) {
            divisor *= 10;
        }
        return (cmd / divisor) % 10 == 1;
    }
};

std::vector<long long> load_code(const std::string& filename) {
    std::ifstream file(filename);
    std::string line;
    std::getline(file, line);
    file.close();

    std::vector<long long> code;
    std::stringstream ss(line);
    std::string segment;

    while (std::getline(ss, segment, ',')) {
        code.push_back(std::stoll(segment));
    }
    return code;
}

long long run_amplifiers(const std::vector<int>& phase, const std::vector<long long>& initial_code) {
    std::vector<std::queue<long long>> queues(6);
    std::vector<std::thread> threads;

    for (int i = 0; i < 5; ++i) {
        queues[i].push(phase[i]);
        threads.emplace_back([i, &initial_code, &queues]() {
            VM vm(initial_code, queues[i], queues[i + 1]);
            vm.run();
        });
    }

    queues[0].push(0);

    for (auto& t : threads) {
        t.join();
    }

    return queues[5].front();
}

int main() {
    std::vector<long long> initial_code = load_code("input.txt");

    std::vector<int> phase_settings = {0, 1, 2, 3, 4};
    long long max_output = 0;

    std::sort(phase_settings.begin(), phase_settings.end());
    do {
        long long output = run_amplifiers(phase_settings, initial_code);
        max_output = std::max(max_output, output);
    } while (std::next_permutation(phase_settings.begin(), phase_settings.end()));

    std::cout << max_output << std::endl;

    return 0;
}
