
/**
 * D program to solve the Advent of Code Day 3 challenge: Gear Ratios.
 *
 * This program reads an engine schematic from "input.txt". It identifies numbers
 * that are part of a "gear" system. A gear is a '*' symbol adjacent to exactly
 * two numbers. The program calculates the "gear ratio" for each gear by
 * multiplying its two adjacent numbers, and then sums up all these ratios.
 *
 * The solution is designed to be efficient, processing the entire grid in a
 * single pass to find all numbers and their relationships with potential gears.
 * The final result is printed to standard output.
 */
import std.stdio;
import std.file;
import std.string;
import std.ascii;
import std.conv;
import std.algorithm;
import std.range;

/// A simple struct to represent a 2D point, used as a key for the gear map.
struct Point
{
    size_t r, c;
}

/// Main entry point of the program.
void main()
{
    // --- Input Reading ---
    string[] lines;
    try
    {
        // Read the entire file, strip leading/trailing whitespace, and split into lines.
        lines = readText("input.txt").strip.splitLines;
    }
    catch (FileException e)
    {
        stderr.writeln("Error: Cannot read 'input.txt'. ", e.msg);
        return;
    }

    // Handle empty input file.
    if (lines.empty)
    {
        writeln(0);
        return;
    }

    // --- Data Structures ---
    // An associative array to map gear locations ('*') to a list of adjacent part numbers.
    long[][Point] gearParts;

    const size_t numRows = lines.length;
    const size_t numCols = lines[0].length;

    // --- Grid Traversal and Parsing ---
    // Iterate through each character of the grid to find numbers.
    for (size_t r = 0; r < numRows; ++r)
    {
        // The column index 'c' is advanced manually inside the loop
        // to skip over numbers that have already been processed.
        for (size_t c = 0; c < numCols; /* no-op */)
        {
            // We are only interested in the start of a number.
            if (!isDigit(lines[r][c]))
            {
                c++;
                continue;
            }

            // Found a digit. Parse the full number by finding its end.
            size_t startC = c;
            size_t endC = c;
            while (endC + 1 < numCols && isDigit(lines[r][endC + 1]))
            {
                endC++;
            }
            auto number = to!long(lines[r][startC .. endC + 1]);

            // --- Adjacency Check ---
            // Check the bounding box around the number for symbols.
            // The bounds are calculated carefully to prevent out-of-bounds access.
            const size_t rStart = (r > 0) ? r - 1 : 0;
            const size_t rEnd = (r + 1 < numRows) ? r + 1 : numRows - 1;
            const size_t cStart = (startC > 0) ? startC - 1 : 0;
            const size_t cEnd = (endC + 1 < numCols) ? endC + 1 : numCols - 1;

            for (size_t nr = rStart; nr <= rEnd; ++nr)
            {
                for (size_t nc = cStart; nc <= cEnd; ++nc)
                {
                    // A gear is a '*' symbol. If one is adjacent,
                    // record the current number as being part of that gear.
                    if (lines[nr][nc] == '*')
                    {
                        gearParts[Point(nr, nc)] ~= number;
                    }
                }
            }

            // Advance the column index past the number we just processed.
            c = endC + 1;
        }
    }

    // --- Calculation and Output ---
    // Calculate the sum of all gear ratios using a functional programming style.
    // A gear is valid only if it's adjacent to exactly two numbers.
    const long totalGearRatio = gearParts.values
                                         .filter!(parts => parts.length == 2)
                                         .map!(parts => parts[0] * parts[1])
                                         .sum;

    // Print the final result for Part Two.
    writeln(totalGearRatio);
}
