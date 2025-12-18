
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * Solution for Day 6: Trash Compactor (Part Two)
 * This program reads a math worksheet from input.txt where numbers are given in vertical columns
 * and problems are arranged horizontally. The math is read right-to-left.
 */
public class Solution {
    public static void main(String[] args) {
        String fileName = "input.txt";
        List<String> lines = new ArrayList<>();
        
        // Read worksheet lines from input file
        try (BufferedReader br = new BufferedReader(new FileReader(fileName))) {
            String line;
            while ((line = br.readLine()) != null) {
                lines.add(line);
            }
        } catch (IOException e) {
            // Silently handle IO exceptions
            return;
        }

        if (lines.isEmpty()) return;

        // Determine dimensions of the worksheet grid
        int numRows = lines.size();
        int maxCols = 0;
        for (String line : lines) {
            if (line.length() > maxCols) {
                maxCols = line.length();
            }
        }

        // Fill a 2D grid with the worksheet characters, padding shorter lines with spaces
        char[][] grid = new char[numRows][maxCols];
        for (int i = 0; i < numRows; i++) {
            String line = lines.get(i);
            for (int j = 0; j < maxCols; j++) {
                grid[i][j] = (j < line.length()) ? line.charAt(j) : ' ';
            }
        }

        long grandTotal = 0;

        // Iterate through columns to identify and solve each "problem block"
        for (int j = 0; j < maxCols; j++) {
            // Problems are separated by a full column of spaces
            if (isColumnEmpty(grid, j)) continue;

            int start = j;
            // Identify the range of columns that make up the current problem block
            while (j < maxCols && !isColumnEmpty(grid, j)) {
                j++;
            }
            int end = j;

            // Solve the problem block using the cephalopod math rules (Part 2)
            grandTotal += solveProblem(grid, start, end);
            
            // Move index to the end of the current block
            j = end - 1;
        }

        // Output the grand total to standard output
        System.out.println(grandTotal);
    }

    /**
     * Determines if a vertical column contains only space characters.
     */
    private static boolean isColumnEmpty(char[][] grid, int col) {
        for (int i = 0; i < grid.length; i++) {
            if (grid[i][col] != ' ') return false;
        }
        return true;
    }

    /**
     * Solves a single math problem block found between the specified columns.
     */
    private static long solveProblem(char[][] grid, int start, int end) {
        int numRows = grid.length;
        char operator = '+'; // Default operator, assuming one will be provided
        
        // Find the operator symbol (+ or *) in the very last row of the block
        for (int j = start; j < end; j++) {
            char c = grid[numRows - 1][j];
            if (c == '+' || c == '*') {
                operator = c;
                break;
            }
        }

        // Extract numbers from columns within the block, reading from right to left
        List<Long> numbers = new ArrayList<>();
        for (int j = end - 1; j >= start; j--) {
            StringBuilder columnDigits = new StringBuilder();
            // Digits of a number are vertical; most significant at top, least at bottom
            for (int i = 0; i < numRows - 1; i++) {
                columnDigits.append(grid[i][j]);
            }
            
            String numberStr = columnDigits.toString().trim();
            if (!numberStr.isEmpty()) {
                try {
                    numbers.add(Long.parseLong(numberStr));
                } catch (NumberFormatException e) {
                    // Ignore non-numeric columns if encountered
                }
            }
        }

        if (numbers.isEmpty()) return 0;

        // Perform the arithmetic operation on the gathered numbers
        long result = numbers.get(0);
        for (int i = 1; i < numbers.size(); i++) {
            if (operator == '+') {
                result += numbers.get(i);
            } else if (operator == '*') {
                result *= numbers.get(i);
            }
        }
        return result;
    }
}

