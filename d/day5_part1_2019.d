import std.stdio;
import std.conv;
import std.file;
import std.array;
import std.algorithm;
import std.string;

// Parses the input file and returns an array of integers (the Intcode program)
int[] parseInput(string fileName) {
    string input = readText(fileName).strip();
    return input.split(",").map!(to!int).array;
}

// Determines the parameter value based on its mode
int getParameterValue(in int[] memory, int parameter, int mode) {
    // mode == 0 for position mode, mode == 1 for immediate mode
    return mode == 0 ? memory[parameter] : parameter;
}

// Executes the Intcode program
void executeIntcode(int[] memory) {
    int i = 0; // Instruction pointer
    while (i < memory.length) {
        int instruction = memory[i];
        int opcode = instruction % 100; // Last two digits for opcode
        int mode1 = (instruction / 100) % 10;
        int mode2 = (instruction / 1000) % 10;
        // Parameters that an instruction writes to will never be in immediate mode

        switch (opcode) {
            case 1: // Addition
                memory[memory[i+3]] = getParameterValue(memory, memory[i+1], mode1) + getParameterValue(memory, memory[i+2], mode2);
                i += 4;
                break;
            case 2: // Multiplication
                memory[memory[i+3]] = getParameterValue(memory, memory[i+1], mode1) * getParameterValue(memory, memory[i+2], mode2);
                i += 4;
                break;
            case 3: // Input
                // The problem statement specifies to use 1 as input
                memory[memory[i+1]] = 1;
                i += 2;
                break;
            case 4: // Output
                writeln(getParameterValue(memory, memory[i+1], mode1));
                i += 2;
                break;
            case 99: // Halt
                return;
            default:
                writeln("Error: Unknown opcode encountered: ", opcode);
                return;
        }
    }
}

void main() {
    // Load the Intcode program from the input file
    int[] memory = parseInput("input.txt");
    // Execute the Intcode program
    executeIntcode(memory);
}
