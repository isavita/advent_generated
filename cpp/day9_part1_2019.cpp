
#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <sstream>
#include <unordered_map>
#include <stdexcept>
#include <algorithm> // Required for std::reverse

using namespace std;

long long get_param(long long ip, long long offset, long long relative_base, const string& modes_str, unordered_map<long long, long long>& memory) {
    long long mode = 0;
    if (offset <= modes_str.length()) {
        mode = modes_str[modes_str.length() - offset] - '0';
    }

    long long param_addr = ip + offset;
    long long param_val = memory[param_addr]; // Default to 0 if not present

    if (mode == 0) { // Position Mode
        return memory[param_val];
    } else if (mode == 1) { // Immediate Mode
        return param_val;
    } else if (mode == 2) { // Relative Mode
        return memory[relative_base + param_val];
    } else {
        throw runtime_error("unknown parameter mode");
    }
}

void set_param(long long ip, long long offset, long long value, long long relative_base, const string& modes_str, unordered_map<long long, long long>& memory) {
    long long mode = 0;
    if (offset <= modes_str.length()) {
        mode = modes_str[modes_str.length() - offset] - '0';
    }

    long long param_addr = ip + offset;
    long long param_val = memory[param_addr]; // Default to 0 if not present

    if (mode == 0) { // Position Mode
        memory[param_val] = value;
    } else if (mode == 2) { // Relative Mode
        memory[relative_base + param_val] = value;
    }
     else { // Mode 1 is invalid for writing
        throw runtime_error("invalid parameter mode for writing");
    }
}


int main() {
    ios_base::sync_with_stdio(false);
    cin.tie(NULL);

    ifstream file("input.txt");
    if (!file.is_open()) {
        cerr << "Error opening input.txt" << endl;
        return 1;
    }

    string line;
    getline(file, line);
    file.close();

    unordered_map<long long, long long> memory;
    stringstream ss(line);
    string segment;
    long long i = 0;
    while (getline(ss, segment, ',')) {
        try {
            memory[i++] = stoll(segment);
        } catch (const std::invalid_argument& e) {
             cerr << "Invalid number format in input: " << segment << endl;
             return 1;
        } catch (const std::out_of_range& e) {
             cerr << "Number out of range in input: " << segment << endl;
            return 1;
        }
    }

    long long output = 0;
    long long ip = 0;
    long long relative_base = 0;

    while (true) {
        long long instruction = memory[ip]; // Default 0 if ip not present
        long long opcode = instruction % 100;
        string modes_str = to_string(instruction / 100);
        // No need to reverse modes_str, access using length - offset

        if (opcode == 1) { // Add
            long long p1 = get_param(ip, 1, relative_base, modes_str, memory);
            long long p2 = get_param(ip, 2, relative_base, modes_str, memory);
            set_param(ip, 3, p1 + p2, relative_base, modes_str, memory);
            ip += 4;
        } else if (opcode == 2) { // Multiply
            long long p1 = get_param(ip, 1, relative_base, modes_str, memory);
            long long p2 = get_param(ip, 2, relative_base, modes_str, memory);
            set_param(ip, 3, p1 * p2, relative_base, modes_str, memory);
            ip += 4;
        } else if (opcode == 3) { // Input
            set_param(ip, 1, 1, relative_base, modes_str, memory); // Test mode input is 1
            ip += 2;
        } else if (opcode == 4) { // Output
            output = get_param(ip, 1, relative_base, modes_str, memory);
            ip += 2;
        } else if (opcode == 5) { // Jump-if-true
            long long p1 = get_param(ip, 1, relative_base, modes_str, memory);
            long long p2 = get_param(ip, 2, relative_base, modes_str, memory);
            if (p1 != 0) {
                ip = p2;
            } else {
                ip += 3;
            }
        } else if (opcode == 6) { // Jump-if-false
            long long p1 = get_param(ip, 1, relative_base, modes_str, memory);
            long long p2 = get_param(ip, 2, relative_base, modes_str, memory);
            if (p1 == 0) {
                ip = p2;
            } else {
                ip += 3;
            }
        } else if (opcode == 7) { // Less than
            long long p1 = get_param(ip, 1, relative_base, modes_str, memory);
            long long p2 = get_param(ip, 2, relative_base, modes_str, memory);
            set_param(ip, 3, (p1 < p2) ? 1 : 0, relative_base, modes_str, memory);
            ip += 4;
        } else if (opcode == 8) { // Equals
            long long p1 = get_param(ip, 1, relative_base, modes_str, memory);
            long long p2 = get_param(ip, 2, relative_base, modes_str, memory);
            set_param(ip, 3, (p1 == p2) ? 1 : 0, relative_base, modes_str, memory);
            ip += 4;
        } else if (opcode == 9) { // Adjust relative base
            relative_base += get_param(ip, 1, relative_base, modes_str, memory);
            ip += 2;
        } else if (opcode == 99) { // Halt
            cout << output << endl;
            break;
        } else {
             cerr << "unknown opcode: " << opcode << " at ip " << ip << endl;
             return 1; // Error exit
        }
    }

    return 0;
}
