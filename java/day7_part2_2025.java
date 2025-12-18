
import java.util.*;
import java.io.*;

/**
 * This program solves the "many-worlds interpretation" of quantum tachyon splitting
 * as described in the Laboratory challenge. It uses a dynamic programming approach 
 * to count the total number of timelines (paths) a single particle can follow from 
 * its starting position 'S' until it exits the manifold grid.
 */
public class Solution {
    public static void main(String[] args) {
        String inputFileName = "input.txt";
        List<String> manifold = new ArrayList<>();

        // Read the manifold grid from input.txt
        try (BufferedReader reader = new BufferedReader(new FileReader(inputFileName))) {
            String line;
            while ((line = reader.readLine()) != null) {
                // Ignore empty lines to handle potential trailing whitespace or empty inputs
                if (!line.trim().isEmpty()) {
                    manifold.add(line);
                }
            }
        } catch (IOException e) {
            // Exit if file reading fails
            return;
        }

        // Basic validation for empty input
        if (manifold.isEmpty()) return;

        int rowCount = manifold.size();
        int colCount = manifold.get(0).length();

        int startRow = -1;
        int startCol = -1;

        // Locate the starting point 'S' in the manifold diagram
        for (int r = 0; r < rowCount; r++) {
            int c = manifold.get(r).indexOf('S');
            if (c != -1) {
                startRow = r;
                startCol = c;
                break;
            }
        }

        // If no starting point S is found, no timelines can exist
        if (startRow == -1) {
            System.out.println(0);
            return;
        }

        /**
         * Dynamic Programming Table: dp[r][c] stores the number of timelines 
         * currently passing through grid cell (r, c).
         * 
         * As we move row by row from top to bottom, we propagate the count
         * of timelines based on the interaction with space (.) or splitters (^).
         */
        long[][] dp = new long[rowCount][colCount];
        dp[startRow][startCol] = 1;
        long totalTimelines = 0;

        for (int r = 0; r < rowCount; r++) {
            for (int c = 0; c < colCount; c++) {
                // If no timelines are at this specific point, skip it
                if (dp[r][c] == 0) continue;

                int nextRow = r + 1;
                
                // If a particle would move beyond the last row of the grid, 
                // it exits the manifold and the current timeline path is completed.
                if (nextRow >= rowCount) {
                    totalTimelines += dp[r][c];
                } else {
                    // Check the character in the row directly below
                    char nextCell = manifold.get(nextRow).charAt(c);
                    
                    if (nextCell == '^') {
                        /**
                         * A splitter stops the incoming beam and creates two new paths:
                         * one starting at the splitter's immediate left and one at its right.
                         * According to the Many-Worlds interpretation, the timeline splits here.
                         */
                        
                        // Handle the left split: starts at (nextRow, c-1)
                        if (c - 1 < 0) {
                            // Split goes out of manifold boundaries; path is completed.
                            totalTimelines += dp[r][c];
                        } else {
                            dp[nextRow][c - 1] += dp[r][c];
                        }

                        // Handle the right split: starts at (nextRow, c+1)
                        if (c + 1 >= colCount) {
                            // Split goes out of manifold boundaries; path is completed.
                            totalTimelines += dp[r][c];
                        } else {
                            dp[nextRow][c + 1] += dp[r][c];
                        }
                    } else {
                        /**
                         * If the next cell is empty space (.), the particle moves straight down.
                         * Note: 'S' is treated as empty space after the initial entry.
                         */
                        dp[nextRow][c] += dp[r][c];
                    }
                }
            }
        }

        // Output the final count of distinct timelines that successfully exited the manifold
        System.out.println(totalTimelines);
    }
}

