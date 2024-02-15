import std.stdio;
import std.conv;
import std.file;
import std.array;
import std.algorithm.iteration;
import std.string;

void main() {
    // Read the Intcode program from input.txt and strip whitespace
    string input = readText("input.txt").strip();
    // Convert the string of numbers into an array of integers
    int[] codes = input.split(",").map!(a => to!int(a)).array();

    // Restore the gravity assist program to the "1202 program alarm" state
    codes[1] = 12;
    codes[2] = 2;

    // Process the Intcode program
    for (int i = 0; i < codes.length; i += 4) {
        switch (codes[i]) {
            case 1:
                // Addition
                codes[codes[i+3]] = codes[codes[i+1]] + codes[codes[i+2]];
                break;
            case 2:
                // Multiplication
                codes[codes[i+3]] = codes[codes[i+1]] * codes[codes[i+2]];
                break;
            case 99:
                // Halt
                writeln(codes[0]);
                return;
            default:
                // Unknown opcode
                writeln("Error: Unknown opcode encountered.");
                return;
        }
    }
}

