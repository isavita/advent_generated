
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

/**
 * Solves the Printing Department grid challenge.
 * A roll of paper (@) is removed if it has fewer than 4 adjacent rolls.
 * This process repeats until no more rolls can be removed.
 */
public class PrintingDepartment {

    public static void main(String[] args) {
        try {
            // Read all lines from input.txt
            List<String> lines = Files.readAllLines(Paths.get("input.txt"));
            if (lines.isEmpty()) return;

            int rows = lines.size();
            int cols = lines.get(0).length();
            char[][] grid = new char[rows][cols];

            // Initialize the grid
            for (int i = 0; i < rows; i++) {
                grid[i] = lines.get(i).toCharArray();
            }

            int totalRemoved = 0;
            boolean changed;

            // Iteratively remove rolls that satisfy the condition
            do {
                changed = false;
                List<int[]> toRemove = new ArrayList<>();

                // Scan grid for accessible rolls (fewer than 4 neighbors)
                for (int r = 0; r < rows; r++) {
                    for (int c = 0; c < cols; c++) {
                        if (grid[r][c] == '@') {
                            if (countAdjacentRolls(grid, r, c) < 4) {
                                toRemove.add(new int[]{r, c});
                            }
                        }
                    }
                }

                // If rolls were identified, remove them and mark the grid as changed
                if (!toRemove.isEmpty()) {
                    totalRemoved += toRemove.size();
                    for (int[] pos : toRemove) {
                        grid[pos[0]][pos[1]] = '.';
                    }
                    changed = true;
                }
            } while (changed);

            // Print the final result to standard output
            System.out.println(totalRemoved);

        } catch (IOException e) {
            System.err.println("Could not read file input.txt: " + e.getMessage());
        }
    }

    /**
     * Counts the number of '@' characters in the 8 surrounding cells.
     */
    private static int countAdjacentRolls(char[][] grid, int r, int c) {
        int count = 0;
        int rows = grid.length;
        int cols = grid[0].length;

        for (int dr = -1; dr <= 1; dr++) {
            for (int dc = -1; dc <= 1; dc++) {
                // Skip the current cell itself
                if (dr == 0 && dc == 0) continue;

                int nr = r + dr;
                int nc = c + dc;

                // Check boundaries and increment count if a roll exists
                if (nr >= 0 && nr < rows && nc >= 0 && nc < cols) {
                    if (grid[nr][nc] == '@') {
                        count++;
                    }
                }
            }
        }
        return count;
    }
}
