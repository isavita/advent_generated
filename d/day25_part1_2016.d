
import std.stdio;
import std.string;
import std.conv;
import std.file;
import std.regex;

/**
 * Solves the Advent of Code Day 25 challenge by reverse-engineering
 * the assembunny code's logic instead of direct simulation.
 */
void main() {
    try {
        // Read the entire program from the input file.
        auto programText = readText("input.txt");

        // Use compile-time regular expressions to efficiently find the two
        // constants that the assembunny code multiplies together.
        auto match1 = match(programText, ctRegex!(`cpy (\d+) c`));
        auto match2 = match(programText, ctRegex!(`cpy (\d+) b`));

        if (match1.empty || match2.empty) {
            stderr.writeln("Error: Could not parse the required constants from 'input.txt'.");
            stderr.writeln("The program expects to find lines like 'cpy 123 c' and 'cpy 456 b'.");
            return;
        }

        // Convert the captured string parts to long integers.
        auto const1 = to!long(match1.captures[1]);
        auto const2 = to!long(match2.captures[1]);

        // This is the number that gets added to the initial value of 'a'.
        long magicNumber = const1 * const2;

        // We need to find the smallest number `N` of the form '...1010' (binary)
        // such that `N > magicNumber`. We start with the smallest such number, 2 (binary '10').
        long n = 2;

        // Keep generating the next number in the '...1010' sequence until it's
        // larger than the magic number calculated from the input.
        while (n <= magicNumber) {
            // The next number is found by shifting left 2 bits and adding '10' (binary),
            // which is arithmetically equivalent to `n * 4 + 2`.
            n = (n * 4) + 2;
        }

        // The required lowest positive integer for 'a' is the difference.
        long result = n - magicNumber;

        // Print the final answer to standard output.
        writeln(result);

    } catch (FileException e) {
        stderr.writeln("Error reading file 'input.txt': ", e.msg);
    }
}
