
#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <map>
#include <deque>
#include <sstream>
#include <cctype> // For std::isalpha

// Enum for command types to avoid string comparisons in the loop
enum CmdType { SND, SET, ADD, MUL, MOD, RCV, JGZ };

// Struct to store parsed arguments, indicating if it's a register or an immediate value
struct Argument {
    bool is_reg;     // True if it's a register name, false if it's an immediate value
    char reg_char;   // Stores the register character if is_reg is true
    long long value; // Stores the immediate numeric value if is_reg is false
};

// Helper function to parse a string into an Argument struct
Argument parse_argument_string(const std::string& s) {
    Argument arg;
    if (!s.empty() && std::isalpha(s[0])) { // Check if the first character is an alphabet (a register)
        arg.is_reg = true;
        arg.reg_char = s[0];
    } else { // Otherwise, it's a numeric value
        arg.is_reg = false;
        arg.value = std::stoll(s); // Convert string to long long
    }
    return arg;
}

// Main instruction struct to hold parsed command and arguments
struct Instruction {
    CmdType cmd;
    Argument arg1;
    Argument arg2; // Used only for commands that take two arguments (SET, ADD, MUL, MOD, JGZ)
};

// Helper function to get the numeric value of an argument.
// If the argument is a register, it fetches its value from the registers map.
// If the register doesn't exist, std::map::operator[] will create it and default-initialize to 0,
// matching the problem's "registers start with a value of 0" rule.
long long get_argument_value(const Argument& arg, std::map<char, long long>& registers) {
    if (arg.is_reg) {
        return registers[arg.reg_char];
    } else {
        return arg.value;
    }
}

int main() {
    std::vector<Instruction> instructions;
    std::ifstream file("input.txt");
    std::string line;

    // Parse input file and store instructions
    while (std::getline(file, line)) {
        std::vector<std::string> tokens;
        std::string token;
        std::istringstream iss(line);
        while (iss >> token) {
            tokens.push_back(token);
        }

        if (tokens.empty()) continue;

        Instruction instr;
        const std::string& cmd_str = tokens[0];

        if (cmd_str == "snd") {
            instr.cmd = SND;
            instr.arg1 = parse_argument_string(tokens[1]);
        } else if (cmd_str == "set") {
            instr.cmd = SET;
            instr.arg1 = parse_argument_string(tokens[1]);
            instr.arg2 = parse_argument_string(tokens[2]);
        } else if (cmd_str == "add") {
            instr.cmd = ADD;
            instr.arg1 = parse_argument_string(tokens[1]);
            instr.arg2 = parse_argument_string(tokens[2]);
        } else if (cmd_str == "mul") {
            instr.cmd = MUL;
            instr.arg1 = parse_argument_string(tokens[1]);
            instr.arg2 = parse_argument_string(tokens[2]);
        } else if (cmd_str == "mod") {
            instr.cmd = MOD;
            instr.arg1 = parse_argument_string(tokens[1]);
            instr.arg2 = parse_argument_string(tokens[2]);
        } else if (cmd_str == "rcv") {
            instr.cmd = RCV;
            instr.arg1 = parse_argument_string(tokens[1]);
        } else if (cmd_str == "jgz") {
            instr.cmd = JGZ;
            instr.arg1 = parse_argument_string(tokens[1]);
            instr.arg2 = parse_argument_string(tokens[2]);
        }
        instructions.push_back(instr);
    }
    file.close();

    // Initialize program states
    std::map<char, long long> registers0;
    std::map<char, long long> registers1;
    registers0['p'] = 0; // Program 0 starts with register 'p' at 0
    registers1['p'] = 1; // Program 1 starts with register 'p' at 1

    std::deque<long long> queue0; // Messages from program 1 to program 0
    std::deque<long long> queue1; // Messages from program 0 to program 1

    long long send_count1 = 0; // Counter for values sent by Program 1
    long long i0 = 0;          // Instruction pointer for program 0
    long long i1 = 0;          // Instruction pointer for program 1

    bool deadlock0 = false; // True if Program 0 is stalled waiting for input
    bool deadlock1 = false; // True if Program 1 is stalled waiting for input

    // Main simulation loop: continues until both programs are deadlocked
    while (!(deadlock0 && deadlock1)) {
        deadlock0 = true; // Assume deadlock initially for current turn
        deadlock1 = true;

        // --- Program 0 execution ---
        while (i0 >= 0 && i0 < instructions.size()) {
            const Instruction& current_instr = instructions[i0];
            bool jumped = false; // Flag to indicate if a jump instruction modified i0

            switch (current_instr.cmd) {
                case SND:
                    queue1.push_back(get_argument_value(current_instr.arg1, registers0));
                    break;
                case SET:
                    registers0[current_instr.arg1.reg_char] = get_argument_value(current_instr.arg2, registers0);
                    break;
                case ADD:
                    registers0[current_instr.arg1.reg_char] += get_argument_value(current_instr.arg2, registers0);
                    break;
                case MUL:
                    registers0[current_instr.arg1.reg_char] *= get_argument_value(current_instr.arg2, registers0);
                    break;
                case MOD:
                    registers0[current_instr.arg1.reg_char] %= get_argument_value(current_instr.arg2, registers0);
                    break;
                case RCV:
                    if (queue0.empty()) {
                        goto end_program0_turn; // Break out of inner while loop if no message
                    }
                    registers0[current_instr.arg1.reg_char] = queue0.front();
                    queue0.pop_front();
                    break;
                case JGZ:
                    if (get_argument_value(current_instr.arg1, registers0) > 0) {
                        i0 += get_argument_value(current_instr.arg2, registers0);
                        jumped = true; // i0 was changed by jump, don't increment normally
                    }
                    break;
            }
            if (!jumped) {
                i0++; // Move to next instruction if no jump occurred
            }
            deadlock0 = false; // Program 0 executed an instruction, not deadlocked
        }
        end_program0_turn:; // Label for goto

        // --- Program 1 execution ---
        while (i1 >= 0 && i1 < instructions.size()) {
            const Instruction& current_instr = instructions[i1];
            bool jumped = false; // Flag to indicate if a jump instruction modified i1

            switch (current_instr.cmd) {
                case SND:
                    queue0.push_back(get_argument_value(current_instr.arg1, registers1));
                    send_count1++; // Increment counter for messages sent by Program 1
                    break;
                case SET:
                    registers1[current_instr.arg1.reg_char] = get_argument_value(current_instr.arg2, registers1);
                    break;
                case ADD:
                    registers1[current_instr.arg1.reg_char] += get_argument_value(current_instr.arg2, registers1);
                    break;
                case MUL:
                    registers1[current_instr.arg1.reg_char] *= get_argument_value(current_instr.arg2, registers1);
                    break;
                case MOD:
                    registers1[current_instr.arg1.reg_char] %= get_argument_value(current_instr.arg2, registers1);
                    break;
                case RCV:
                    if (queue1.empty()) {
                        goto end_program1_turn; // Break out of inner while loop if no message
                    }
                    registers1[current_instr.arg1.reg_char] = queue1.front();
                    queue1.pop_front();
                    break;
                case JGZ:
                    if (get_argument_value(current_instr.arg1, registers1) > 0) {
                        i1 += get_argument_value(current_instr.arg2, registers1);
                        jumped = true; // i1 was changed by jump, don't increment normally
                    }
                    break;
            }
            if (!jumped) {
                i1++; // Move to next instruction if no jump occurred
            }
            deadlock1 = false; // Program 1 executed an instruction, not deadlocked
        }
        end_program1_turn:; // Label for goto
    }

    // Print the final result
    std::cout << send_count1 << std::endl;

    return 0;
}
