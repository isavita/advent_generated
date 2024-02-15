import std.stdio;
import std.conv;
import std.file;
import std.array;
import std.algorithm;
import std.string;

int[] parseInput(string fileName) {
    string input = readText(fileName).strip();
    return input.split(",").map!(to!int).array;
}

int getParameterValue(in int[] memory, int parameter, int mode) {
    return mode == 0 ? memory[parameter] : parameter;
}

void executeIntcode(int[] memory) {
    int i = 0;
    while (i < memory.length) {
        int instruction = memory[i];
        int opcode = instruction % 100;
        int mode1 = (instruction / 100) % 10;
        int mode2 = (instruction / 1000) % 10;

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
                memory[memory[i+1]] = 5; // Provide 5 as input for the ship's thermal radiator controller
                i += 2;
                break;
            case 4: // Output
                writeln(getParameterValue(memory, memory[i+1], mode1));
                i += 2;
                break;
            case 5: // Jump-if-true
                if (getParameterValue(memory, memory[i+1], mode1) != 0) {
                    i = getParameterValue(memory, memory[i+2], mode2);
                } else {
                    i += 3;
                }
                break;
            case 6: // Jump-if-false
                if (getParameterValue(memory, memory[i+1], mode1) == 0) {
                    i = getParameterValue(memory, memory[i+2], mode2);
                } else {
                    i += 3;
                }
                break;
            case 7: // Less than
                memory[memory[i+3]] = getParameterValue(memory, memory[i+1], mode1) < getParameterValue(memory, memory[i+2], mode2) ? 1 : 0;
                i += 4;
                break;
            case 8: // Equals
                memory[memory[i+3]] = getParameterValue(memory, memory[i+1], mode1) == getParameterValue(memory, memory[i+2], mode2) ? 1 : 0;
                i += 4;
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
    int[] memory = parseInput("input.txt");
    executeIntcode(memory);
}

