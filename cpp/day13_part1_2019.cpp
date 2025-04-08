
#include <iostream>
#include <vector>
#include <map>
#include <fstream>
#include <string>
#include <sstream>
#include <stdexcept>
#include <vector>

using namespace std;

using ll = long long;

enum Mode {
    POSITION = 0,
    IMMEDIATE = 1,
    RELATIVE = 2
};

enum Opcode {
    ADD = 1,
    MUL = 2,
    INPUT = 3,
    OUTPUT = 4,
    JT = 5,
    JF = 6,
    LT = 7,
    EQ = 8,
    RBO = 9,
    HALT = 99
};

struct Instruction {
    int opcode;
    vector<int> modes;
};

Instruction decode(ll n) {
    Instruction instr;
    instr.opcode = n % 100;
    n /= 100;
    instr.modes.resize(3);
    instr.modes[0] = n % 10;
    n /= 10;
    instr.modes[1] = n % 10;
    n /= 10;
    instr.modes[2] = n % 10;
    return instr;
}

struct Machine {
    map<ll, ll> data;
    ll ip = 0;
    ll rel_base = 0;
    vector<ll>& out_stream;
    // No input stream needed for this specific problem's context

    Machine(const vector<ll>& program, vector<ll>& output) : out_stream(output) {
        for (size_t i = 0; i < program.size(); ++i) {
            data[i] = program[i];
        }
    }

    ll read_mem(ll address) {
        if (address < 0) {
             throw runtime_error("Negative address read attempt");
        }
        return data.count(address) ? data[address] : 0;
    }

    void write_mem(ll address, ll value) {
         if (address < 0) {
             throw runtime_error("Negative address write attempt");
         }
         data[address] = value;
    }

    ll get_param_value(ll param_index, int mode) {
        ll param = read_mem(ip + param_index);
        if (mode == Mode::IMMEDIATE) {
            return param;
        } else if (mode == Mode::POSITION) {
            return read_mem(param);
        } else if (mode == Mode::RELATIVE) {
            return read_mem(rel_base + param);
        }
        throw runtime_error("Unknown parameter mode for get");
    }

    ll get_param_address(ll param_index, int mode) {
         ll param = read_mem(ip + param_index);
         if (mode == Mode::POSITION) {
             return param;
         } else if (mode == Mode::RELATIVE) {
             return rel_base + param;
         }
         throw runtime_error("Invalid parameter mode for set");
    }


    bool step() {
        Instruction instr = decode(read_mem(ip));
        int op = instr.opcode;
        const auto& modes = instr.modes;

        switch (op) {
            case Opcode::ADD: {
                ll val1 = get_param_value(1, modes[0]);
                ll val2 = get_param_value(2, modes[1]);
                ll dest_addr = get_param_address(3, modes[2]);
                write_mem(dest_addr, val1 + val2);
                ip += 4;
                break;
            }
            case Opcode::MUL: {
                ll val1 = get_param_value(1, modes[0]);
                ll val2 = get_param_value(2, modes[1]);
                ll dest_addr = get_param_address(3, modes[2]);
                write_mem(dest_addr, val1 * val2);
                ip += 4;
                break;
            }
            case Opcode::INPUT: {
                ll dest_addr = get_param_address(1, modes[0]);
                // This problem assumes no input is needed. If input were required,
                // this would need modification to read from an input source.
                // For now, we'll throw as the Python code would implicitly fail.
                throw runtime_error("INPUT opcode executed but no input provided");
                // write_mem(dest_addr, inputValue); // Example if input existed
                // ip += 2;
                break;
            }
            case Opcode::OUTPUT: {
                ll val = get_param_value(1, modes[0]);
                out_stream.push_back(val);
                ip += 2;
                break;
            }
            case Opcode::JT: {
                ll cond = get_param_value(1, modes[0]);
                ll jump_target = get_param_value(2, modes[1]);
                if (cond != 0) {
                    ip = jump_target;
                } else {
                    ip += 3;
                }
                break;
            }
            case Opcode::JF: {
                ll cond = get_param_value(1, modes[0]);
                ll jump_target = get_param_value(2, modes[1]);
                if (cond == 0) {
                    ip = jump_target;
                } else {
                    ip += 3;
                }
                break;
            }
            case Opcode::LT: {
                ll val1 = get_param_value(1, modes[0]);
                ll val2 = get_param_value(2, modes[1]);
                ll dest_addr = get_param_address(3, modes[2]);
                write_mem(dest_addr, (val1 < val2) ? 1 : 0);
                ip += 4;
                break;
            }
            case Opcode::EQ: {
                ll val1 = get_param_value(1, modes[0]);
                ll val2 = get_param_value(2, modes[1]);
                ll dest_addr = get_param_address(3, modes[2]);
                write_mem(dest_addr, (val1 == val2) ? 1 : 0);
                ip += 4;
                break;
            }
            case Opcode::RBO: {
                ll val = get_param_value(1, modes[0]);
                rel_base += val;
                ip += 2;
                break;
            }
            case Opcode::HALT:
                return false;
            default:
                throw runtime_error("Unknown opcode encountered");
        }
        return true;
    }

    void run() {
        while (step()) {
            // Continue execution
        }
    }
};

ll count_blocks(const vector<ll>& program) {
    vector<ll> out_stream;
    Machine m(program, out_stream);
    m.run();

    ll block_count = 0;
    for (size_t i = 2; i < out_stream.size(); i += 3) {
        if (out_stream[i] == 2) {
            block_count++;
        }
    }
    return block_count;
}

int main() {
    ios_base::sync_with_stdio(false);
    cin.tie(NULL);

    ifstream infile("input.txt");
    if (!infile) {
        cerr << "Error opening input.txt" << endl;
        return 1;
    }

    string line;
    if (!getline(infile, line)) {
         cerr << "Error reading line from input.txt" << endl;
         return 1;
    }
    infile.close();

    stringstream ss(line);
    vector<ll> program;
    string segment;
    while (getline(ss, segment, ',')) {
        try {
            program.push_back(stoll(segment));
        } catch (const std::invalid_argument& e) {
            cerr << "Invalid number format in input: " << segment << endl;
            return 1;
        } catch (const std::out_of_range& e) {
             cerr << "Number out of range in input: " << segment << endl;
             return 1;
        }
    }

    try {
        cout << count_blocks(program) << endl;
    } catch (const runtime_error& e) {
        cerr << "Runtime Error: " << e.what() << endl;
        return 1;
    }

    return 0;
}
