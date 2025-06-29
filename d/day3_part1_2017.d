
import std.stdio;
import std.file;
import std.string;
import std.conv;
import std.math;

/**
 * Calculates the Manhattan distance for a given square in the spiral memory.
 *
 * This function uses a mathematical approach to find the distance without
 * building the spiral. It identifies the "ring" the number is on and then
 * calculates its distance from the center based on its position within that ring.
 *
 * Params:
 *   n = The target square number.
 * Returns: The Manhattan distance from square n to square 1.
 */
int calculateSpiralDistance(int n) {
    // The distance for square 1 is a special case.
    if (n == 1) {
        return 0;
    }

    // Step 1: Find the ring 'k' where n is located.
    // 's' is the side length of the smallest square grid containing n.
    // This will be the first odd number >= sqrt(n).
    int s = cast(int)ceil(sqrt(cast(double)n));
    if (s % 2 == 0) {
        s++;
    }
    // 'k' is the ring number, which is also the minimum Manhattan
    // distance for any number in this ring.
    int k = (s - 1) / 2;

    // Step 2: Find the number's position relative to the ring's cycle.
    // The largest number in the previous ring.
    long prevRingMax = cast(long)(s - 2) * (s - 2);
    // The length of one side of the current ring's path.
    int sideLength = s - 1;
    // The number's 1-based index on the current ring's path.
    long cyclePos = n - prevRingMax;

    // Step 3: Calculate the distance from the midpoint of the side.
    // The position on the specific side (0 to sideLength-1).
    int posOnSide = (cyclePos - 1) % sideLength;
    // The index of the midpoint of a side.
    int midPointIndex = k - 1;
    // The distance from the number to the side's midpoint.
    int distFromMid = abs(posOnSide - midPointIndex);

    // The total Manhattan distance is the distance to the ring (k)
    // plus the distance from the side's midpoint.
    return k + distFromMid;
}

/**
 * Main entry point of the program.
 * Reads a number from input.txt, calculates the spiral memory distance,
 * and prints the result to standard output.
 */
void main() {
    try {
        // Read the entire content of input.txt and remove leading/trailing whitespace.
        string input = readText("input.txt").strip();

        // Convert the input string to an integer.
        int targetSquare = to!int(input);

        // Calculate the result using our efficient algorithm.
        int result = calculateSpiralDistance(targetSquare);

        // Print the final answer to standard output.
        writeln(result);

    } catch (FileException e) {
        stderr.writeln("Error: Could not read 'input.txt'. ", e.msg);
    } catch (ConvException e) {
        stderr.writeln("Error: 'input.txt' does not contain a valid integer. ", e.msg);
    }
}

