
import std.stdio;
import std.file;
import std.string;
import std.conv;
import std.ascii;
import std.algorithm.comparison : min, max;

/**
 * Checks if a character is a symbol (i.e., not a digit and not a period).
 */
bool isSymbol(char c) {
    return !isDigit(c) && c != '.';
}

/**
 * Solves the Day 3 Gear Ratios puzzle.
 *
 * This program reads an engine schematic from "input.txt", identifies all "part numbers"
 * (numbers adjacent to a symbol, including diagonally), and prints their sum to standard output.
 */
void main() {
    // 1. Read the entire file and split it into lines.
    // .strip() removes leading/trailing whitespace, including a final newline.
    string[] lines;
    try {
        lines = readText("input.txt").strip.split("\n");
    } catch (FileException e) {
        stderr.writeln("Error: Could not read 'input.txt'. ", e.msg);
        return;
    }

    // Handle empty or malformed input gracefully.
    if (lines.length == 0 || lines[0].length == 0) {
        writeln(0);
        return;
    }

    long totalSum = 0;
    immutable int height = cast(int) lines.length;
    immutable int width = cast(int) lines[0].length;

    // 2. Iterate through each character of the grid.
    for (int r = 0; r < height; ++r) {
        for (int c = 0; c < width; ++c) {
            // 3. If we find the start of a number, process it.
            if (!isDigit(lines[r][c])) {
                continue;
            }

            // Find the full extent of the number in the current row.
            int cEnd = c;
            while (cEnd + 1 < width && isDigit(lines[r][cEnd + 1])) {
                cEnd++;
            }
            // The number is located at lines[r][c .. cEnd + 1]

            // 4. Check the bounding box around the number for any symbols.
            bool isPartNumber = false;
            // Define the search box boundaries, clamping to the grid edges.
            immutable int rStartCheck = max(0, r - 1);
            immutable int rEndCheck = min(height - 1, r + 1);
            immutable int cStartCheck = max(0, c - 1);
            immutable int cEndCheck = min(width - 1, cEnd + 1);

            // Search within the calculated bounding box.
            // A labeled break allows exiting both loops at once for efficiency.
            checkLoop:
            for (int checkR = rStartCheck; checkR <= rEndCheck; ++checkR) {
                for (int checkC = cStartCheck; checkC <= cEndCheck; ++checkC) {
                    if (isSymbol(lines[checkR][checkC])) {
                        isPartNumber = true;
                        break checkLoop; // Found a symbol, no need to check further.
                    }
                }
            }

            // 5. If a symbol was found, parse the number and add it to the sum.
            if (isPartNumber) {
                auto numberSlice = lines[r][c .. cEnd + 1];
                totalSum += to!long(numberSlice);
            }

            // 6. Advance the column index past the number we just processed
            //    to avoid re-counting its digits.
            c = cEnd;
        }
    }

    // 7. Print the final sum.
    writeln(totalSum);
}
