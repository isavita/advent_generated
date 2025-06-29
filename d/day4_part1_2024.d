
import std.stdio;
import std.file;
import std.string;
import std.array;

/**
 * This program solves the Day 4: Ceres Search challenge.
 * It reads a character grid from "input.txt" and counts all occurrences
 * of the word "XMAS" in 8 directions (horizontal, vertical, and diagonal).
 */

/**
 * Counts all occurrences of a given word within a 2D character grid.
 *
 * The search is performed in 8 directions: horizontal, vertical, and diagonal,
 * both forwards and backwards.
 *
 * Params:
 *   grid = The 2D grid of characters, represented as an array of strings.
 *   word = The word to search for.
 *
 * Returns:
 *   The total number of times the word appears in the grid.
 */
long countOccurrences(const string[] grid, const string word) {
    // Ensure the grid and word are not empty to avoid errors.
    if (grid.empty || grid[0].empty || word.empty) {
        return 0;
    }

    // Cache dimensions and length for efficiency.
    immutable height = grid.length;
    immutable width = grid[0].length;
    immutable wordLen = word.length;

    // Define the 8 directions for the search using delta row and delta column.
    // (dr, dc): [-1,-1] [ -1,0] [-1, 1]
    //           [ 0,-1] [start] [ 0, 1]
    //           [ 1,-1] [  1,0] [ 1, 1]
    immutable directions = [
        [-1, -1], [-1, 0], [-1, 1], // Up-Left, Up, Up-Right
        [ 0, -1],          [ 0, 1], // Left, Right
        [ 1, -1], [ 1, 0], [ 1, 1]  // Down-Left, Down, Down-Right
    ];

    long totalCount = 0;

    // Iterate over every cell in the grid as a potential starting point.
    foreach (r; 0 .. height) {
        foreach (c; 0 .. width) {
            // From each starting cell, check all 8 directions.
            foreach (dir; directions) {
                immutable dr = dir[0];
                immutable dc = dir[1];
                bool isMatch = true;

                // Attempt to match the word along the current direction.
                foreach (i; 0 .. wordLen) {
                    immutable currentRow = r + i * dr;
                    immutable currentCol = c + i * dc;

                    // 1. Bounds Check: Ensure we are within the grid.
                    if (currentRow < 0 || currentRow >= height ||
                        currentCol < 0 || currentCol >= width) {
                        isMatch = false;
                        break; // Word goes out of bounds, try next direction.
                    }

                    // 2. Character Check: Ensure the characters match.
                    if (grid[currentRow][currentCol] != word[i]) {
                        isMatch = false;
                        break; // Mismatch, try next direction.
                    }
                }

                // If the inner loop completed without breaking, we found a match.
                if (isMatch) {
                    totalCount++;
                }
            }
        }
    }

    return totalCount;
}

/**
 * Main entry point of the program.
 */
void main() {
    try {
        // 1. Read the grid from the input file.
        // .strip() removes leading/trailing whitespace (including newlines).
        // .splitLines() splits the text into an array of strings.
        auto grid = readText("input.txt").strip.splitLines;

        // 2. Define the target word.
        immutable string wordToFind = "XMAS";

        // 3. Calculate the number of occurrences.
        auto count = countOccurrences(grid, wordToFind);

        // 4. Print the final count to standard output.
        writeln(count);

    } catch (FileException e) {
        // Handle cases where the input file cannot be read.
        stderr.writeln("Error: Could not read 'input.txt'.");
        stderr.writeln(e.msg);
    }
}
