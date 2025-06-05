
#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <sstream>
#include <queue>
#include <mutex>
#include <condition_variable>
#include <thread>
#include <algorithm>
#include <memory> // For std::shared_ptr and std::unique_ptr

template <typename T>
class ThreadSafeQueue {
private:
    std::queue<T> q;
    mutable std::mutex m;
    std::condition_variable cv;

public:
    void push(const T& value) {
        std::lock_guard<std::mutex> lk(m);
        q.push(value);
        cv.notify_one();
    }

    T pop() {
        std::unique_lock<std::mutex> lk(m);
        cv.wait(lk, [this] { return !q.empty(); });
        T value = q.front();
        q.pop();
        return value;
    }

    // No need for empty() in this specific solution, but good practice to have it
    // bool empty() const {
    //     std::lock_guard<std::mutex> lk(m);
    //     return q.empty();
    // }
};

class VM {
public:
    std::vector<int> code;
    int ip;
    std::shared_ptr<ThreadSafeQueue<int>> input_queue;
    std::shared_ptr<ThreadSafeQueue<int>> output_queue;

    VM(const std::vector<int>& initial_code,
       std::shared_ptr<ThreadSafeQueue<int>> in_q,
       std::shared_ptr<ThreadSafeQueue<int>> out_q)
        : code(initial_code), ip(0), input_queue(in_q), output_queue(out_q) {}

    void run() {
        while (true) {
            int instruction = code[ip];
            int opcode = instruction % 100;

            auto get_param = [&](int address, int mode) {
                int param = code[address];
                if (mode == 1) { // Immediate mode
                    return param;
                } else { // Position mode
                    return code[param];
                }
            };

            switch (opcode) {
                case 1: { // ADD
                    int val1 = get_param(ip + 1, (instruction / 100) % 10);
                    int val2 = get_param(ip + 2, (instruction / 1000) % 10);
                    int addr = code[ip + 3]; // Output address is always in position mode
                    code[addr] = val1 + val2;
                    ip += 4;
                    break;
                }
                case 2: { // MUL
                    int val1 = get_param(ip + 1, (instruction / 100) % 10);
                    int val2 = get_param(ip + 2, (instruction / 1000) % 10);
                    int addr = code[ip + 3];
                    code[addr] = val1 * val2;
                    ip += 4;
                    break;
                }
                case 3: { // INPUT
                    int addr = code[ip + 1];
                    code[addr] = input_queue->pop();
                    ip += 2;
                    break;
                }
                case 4: { // OUTPUT
                    int val = get_param(ip + 1, (instruction / 100) % 10);
                    output_queue->push(val);
                    ip += 2;
                    break;
                }
                case 5: { // JUMP IF TRUE
                    int val1 = get_param(ip + 1, (instruction / 100) % 10);
                    int val2 = get_param(ip + 2, (instruction / 1000) % 10);
                    if (val1 != 0) {
                        ip = val2;
                    } else {
                        ip += 3;
                    }
                    break;
                }
                case 6: { // JUMP IF FALSE
                    int val1 = get_param(ip + 1, (instruction / 100) % 10);
                    int val2 = get_param(ip + 2, (instruction / 1000) % 10);
                    if (val1 == 0) {
                        ip = val2;
                    } else {
                        ip += 3;
                    }
                    break;
                }
                case 7: { // LESS THAN
                    int val1 = get_param(ip + 1, (instruction / 100) % 10);
                    int val2 = get_param(ip + 2, (instruction / 1000) % 10);
                    int addr = code[ip + 3];
                    code[addr] = (val1 < val2) ? 1 : 0;
                    ip += 4;
                    break;
                }
                case 8: { // EQUALS
                    int val1 = get_param(ip + 1, (instruction / 100) % 10);
                    int val2 = get_param(ip + 2, (instruction / 1000) % 10);
                    int addr = code[ip + 3];
                    code[addr] = (val1 == val2) ? 1 : 0;
                    ip += 4;
                    break;
                }
                case 99: // HALT
                    return;
                default:
                    return; // Unknown opcode, terminate VM
            }
        }
    }
};

std::vector<int> load_code(const std::string& filename) {
    std::vector<int> code;
    std::ifstream file(filename);
    std::string line;
    if (file.is_open()) {
        std::getline(file, line);
        std::stringstream ss(line);
        std::string segment;
        while (std::getline(ss, segment, ',')) {
            code.push_back(std::stoi(segment));
        }
    }
    return code;
}

int run_loop(const std::vector<int>& phase_settings, const std::vector<int>& initial_code) {
    std::vector<std::shared_ptr<ThreadSafeQueue<int>>> chs(5);
    for (int i = 0; i < 5; ++i) {
        chs[i] = std::make_shared<ThreadSafeQueue<int>>();
    }

    std::vector<std::thread> threads;
    std::vector<std::unique_ptr<VM>> vms; // To manage VM object lifetimes

    for (int i = 0; i < 5; ++i) {
        chs[i]->push(phase_settings[i]);
        auto vm = std::make_unique<VM>(initial_code, chs[i], chs[(i + 1) % 5]);
        threads.emplace_back([vm_ptr = vm.get()]() { vm_ptr->run(); });
        vms.push_back(std::move(vm));
    }

    chs[0]->push(0); // Initial input signal to the first amplifier

    for (std::thread& t : threads) {
        t.join();
    }

    return chs[0]->pop(); // Final output from the last amplifier
}

int main() {
    std::vector<int> initial_code = load_code("input.txt");

    int max_output = 0;
    std::vector<int> phase_settings = {5, 6, 7, 8, 9};

    std::sort(phase_settings.begin(), phase_settings.end());

    do {
        int output = run_loop(phase_settings, initial_code);
        if (output > max_output) {
            max_output = output;
        }
    } while (std::next_permutation(phase_settings.begin(), phase_settings.end()));

    std::cout << max_output << std::endl;

    return 0;
}
