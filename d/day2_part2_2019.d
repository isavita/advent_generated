import std.stdio;
import std.conv;
import std.file;
import std.array;
import std.algorithm.iteration;
import std.string;

// Function to run the Intcode program
int runIntcode(int[] memory) {
    for (int i = 0; i < memory.length; i += 4) {
        switch (memory[i]) {
            case 1:
                memory[memory[i + 3]] = memory[memory[i + 1]] + memory[memory[i + 2]];
                break;
            case 2:
                memory[memory[i + 3]] = memory[memory[i + 1]] * memory[memory[i + 2]];
                break;
            case 99:
                return memory[0];
            default:
                writeln("Error: Unknown opcode encountered.");
                return -1; // Indicate error
        }
    }
    return -1; // Indicate error if exited loop without halting
}

void main() {
    // Read and prepare the Intcode program
    string input = readText("input.txt").strip();
    int[] originalMemory = input.split(",").map!(a => to!int(a)).array();

    // Desired output
    const int targetOutput = 19690720;

    // Try all combinations of noun and verb
    foreach (int noun; 0 .. 100) {
        foreach (int verb; 0 .. 100) {
            // Reset memory to initial state and set noun and verb
            int[] memory = originalMemory.dup; // Duplicate the original memory for each attempt
            memory[1] = noun;
            memory[2] = verb;

            // Run the program with current noun and verb
            int output = runIntcode(memory);

            // Check if the output matches the target
            if (output == targetOutput) {
                // Calculate and print the answer
                writeln(100 * noun + verb);
                return;
            }
        }
    }
}

