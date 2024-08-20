import std.stdio;
import std.algorithm;
import std.conv;
import std.array;
import std.file;

int runProgram(int[] memory, int input1, int input2) {
    int inputCount = 0;
    int output = 0;
    for (int i = 0; i < memory.length;) {
        int opcode = memory[i] % 100;
        int mode1 = (memory[i] / 100) % 10;
        int mode2 = (memory[i] / 1000) % 10;

        int getParam(int index, int mode) {
            return mode == 0 ? memory[memory[index]] : memory[index];
        }

        switch (opcode) {
            case 1:
                memory[memory[i + 3]] = getParam(i + 1, mode1) + getParam(i + 2, mode2);
                i += 4;
                break;
            case 2:
                memory[memory[i + 3]] = getParam(i + 1, mode1) * getParam(i + 2, mode2);
                i += 4;
                break;
            case 3:
                memory[memory[i + 1]] = inputCount == 0 ? input1 : input2;
                inputCount++;
                i += 2;
                break;
            case 4:
                output = getParam(i + 1, mode1);
                i += 2;
                break;
            case 5:
                if (getParam(i + 1, mode1) != 0) {
                    i = getParam(i + 2, mode2);
                } else {
                    i += 3;
                }
                break;
            case 6:
                if (getParam(i + 1, mode1) == 0) {
                    i = getParam(i + 2, mode2);
                } else {
                    i += 3;
                }
                break;
            case 7:
                memory[memory[i + 3]] = getParam(i + 1, mode1) < getParam(i + 2, mode2) ? 1 : 0;
                i += 4;
                break;
            case 8:
                memory[memory[i + 3]] = getParam(i + 1, mode1) == getParam(i + 2, mode2) ? 1 : 0;
                i += 4;
                break;
            case 99:
                return output;
            default:
                writeln("Error: Unknown opcode encountered.");
                return -1;
        }
    }
    return output;
}

int findMaxSignal(int[] program) {
    int maxSignal = 0;
    foreach (perm; [0, 1, 2, 3, 4].permutations()) {
        int signal = 0;
        foreach (phase; perm) {
            signal = runProgram(program.dup, phase, signal);
        }
        if (signal > maxSignal) {
            maxSignal = signal;
        }
    }
    return maxSignal;
}

void main() {
    string input = cast(string)std.file.read("input.txt");
    int[] program = input.split(",").map!(a => to!int(a)).array();

    int maxSignal = findMaxSignal(program);
    writeln(maxSignal);
}