
import std.stdio;
import std.file;
import std.string;
import std.conv;

/**
 * Solves Part 1: Counts all occurrences of "XMAS" in the grid.
 * The word can be found forwards ("XMAS") or backwards ("SAMX") in any of
 * the 8 horizontal, vertical, or diagonal directions.
 */
long solvePart1(immutable string[] grid) {
    if (grid.empty || grid[0].empty) {
        return 0;
    }

    immutable height = grid.length;
    immutable width = grid[0].length;
    
    // We search for the word and its reverse to handle all directions.
    immutable words = ["XMAS", "SAMX"];
    immutable wordLen = cast(int)words[0].length;
    long count = 0;

    // To avoid double-counting (e.g., finding "XMAS" left-to-right and
    // "SAMX" right-to-left for the same set of letters), we only search in 4
    // unique directions. These 4 directions, when combined with checking for
    // the reversed word, cover all 8 possible orientations exactly once.
    immutable int[2][] directions = [
        [0, 1],   // Right (covers horizontal L->R and R->L)
        [1, 0],   // Down (covers vertical T->B and B->T)
        [1, 1],   // Down-Right (covers diagonal TL->BR and BR->TL)
        [1, -1]   // Down-Left (covers diagonal TR->BL and BL->TR)
    ];

    // Check for each word form ("XMAS" and "SAMX").
    foreach (word; words) {
        // Iterate over every possible starting cell in the grid.
        foreach (r_start; 0 .. height) {
            foreach (c_start; 0 .. width) {
                // Check in each of the 4 cardinal directions.
                foreach (dir; directions) {
                    immutable dr = dir[0];
                    immutable dc = dir[1];
                    
                    // Pre-check if the full word would go out of bounds.
                    immutable r_end = r_start + dr * (wordLen - 1);
                    immutable c_end = c_start + dc * (wordLen - 1);
                    if (r_end < 0 || r_end >= height || c_end < 0 || c_end >= width) {
                        continue;
                    }

                    // Check if the characters in the grid match the word.
                    bool found = true;
                    foreach (i; 0 .. wordLen) {
                        if (grid[r_start + i * dr][c_start + i * dc] != word[i]) {
                            found = false;
                            break;
                        }
                    }

                    if (found) {
                        count++;
                    }
                }
            }
        }
    }
    return count;
}

/**
 * Solves Part 2: Counts all "X-MAS" patterns.
 * An X-MAS is a 3x3 pattern centered on 'A', where the two diagonals
 * each form "MAS" or "SAM".
 * e.g., M . S
 *       . A .
 *       M . S
 */
long solvePart2(immutable string[] grid) {
    // The pattern is 3x3, so the grid must be at least that large.
    if (grid.length < 3 || grid[0].length < 3) {
        return 0;
    }

    immutable height = grid.length;
    immutable width = grid[0].length;
    long count = 0;

    // Iterate through all possible center cells ('A') of the X pattern.
    // The loops are bounded to avoid checking near the edges where a
    // full 3x3 pattern cannot exist.
    foreach (r; 1 .. height - 1) {
        foreach (c; 1 .. width - 1) {
            // The center of the X must be 'A'.
            if (grid[r][c] != 'A') {
                continue;
            }

            // Get the four corner characters relative to the center 'A'.
            immutable tl = grid[r - 1][c - 1]; // top-left
            immutable tr = grid[r - 1][c + 1]; // top-right
            immutable bl = grid[r + 1][c - 1]; // bottom-left
            immutable br = grid[r + 1][c + 1]; // bottom-right

            // Check if the top-left to bottom-right diagonal forms "MAS" or "SAM".
            immutable bool diag1_valid = (tl == 'M' && br == 'S') || (tl == 'S' && br == 'M');
            
            // Check if the top-right to bottom-left diagonal forms "MAS" or "SAM".
            immutable bool diag2_valid = (tr == 'M' && bl == 'S') || (tr == 'S' && bl == 'M');

            // If both diagonals are valid words, we've found an X-MAS.
            if (diag1_valid && diag2_valid) {
                count++;
            }
        }
    }
    return count;
}

/**
 * Main entry point of the program.
 * Reads the puzzle input, solves both parts, and prints the results.
 */
void main() {
    try {
        // Read the word search grid from input.txt.
        // strip() removes leading/trailing whitespace, and splitLines()
        // creates an array of strings, one for each line.
        immutable grid = readText("input.txt").strip.splitLines;

        if (grid.empty) {
            writeln("Input file is empty.");
            return;
        }

        // Solve and print the answer for Part 1.
        const part1_answer = solvePart1(grid);
        writeln("--- Day 4: Ceres Search ---");
        writeln("Part 1: Total 'XMAS' occurrences: ", part1_answer);

        // Solve and print the answer for Part 2.
        const part2_answer = solvePart2(grid);
        writeln("Part 2: Total 'X-MAS' occurrences: ", part2_answer);

    } catch (FileException e) {
        stderr.writeln("Error: Cannot read file 'input.txt'.");
        stderr.writeln(e.msg);
    }
}
