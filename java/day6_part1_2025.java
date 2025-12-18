
import java.io.IOException;
import java.math.BigInteger;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

/**
 * TrashCompactor - Day 6 Challenge
 * This program solves the cephalopod math worksheet by identifying problems 
 * arranged in vertical column groups and performing calculations based on 
 * the operator found at the bottom of each group.
 */
public class TrashCompactor {

    public static void main(String[] args) {
        try {
            // Read all lines from input.txt
            List<String> lines = Files.readAllLines(Paths.get("input.txt"));
            if (lines == null || lines.isEmpty()) return;

            // Determine the width of the worksheet grid
            int maxWidth = 0;
            for (String line : lines) {
                maxWidth = Math.max(maxWidth, line.length());
            }

            BigInteger grandTotal = BigInteger.ZERO;
            int groupStart = -1;

            // Scan column by column to identify problem groups
            // A problem is a set of columns containing characters, separated by a full column of spaces
            for (int col = 0; col <= maxWidth; col++) {
                boolean empty = true;
                // Check if the current column is empty across all rows
                for (String line : lines) {
                    if (col < line.length() && line.charAt(col) != ' ') {
                        empty = false;
                        break;
                    }
                }

                if (!empty && groupStart == -1) {
                    // Marker for the start of a problem block
                    groupStart = col;
                } else if (empty && groupStart != -1) {
                    // Reached a separator or end of file; process the identified group
                    grandTotal = grandTotal.add(calculateProblem(lines, groupStart, col - 1));
                    groupStart = -1;
                }
            }

            // Output the final sum to standard output
            System.out.println(grandTotal.toString());

        } catch (IOException e) {
            // File reading errors
            System.err.println("Error reading input.txt: " + e.getMessage());
        }
    }

    /**
     * Extracts numbers and the operator from a range of columns and calculates the result.
     * @param lines The raw lines of the worksheet.
     * @param start The starting column index for this problem.
     * @param end The ending column index for this problem.
     * @return The calculated BigInteger result for the problem.
     */
    private static BigInteger calculateProblem(List<String> lines, int start, int end) {
        char operator = '+'; // Default to addition
        
        // The last row in the worksheet contains the operator for each problem
        String lastLine = lines.get(lines.size() - 1);
        for (int j = start; j <= end; j++) {
            if (j < lastLine.length()) {
                char c = lastLine.charAt(j);
                if (c == '+' || c == '*') {
                    operator = c;
                    break;
                }
            }
        }

        List<BigInteger> numbers = new ArrayList<>();
        // All rows above the last contain segments of the numbers for this problem
        for (int i = 0; i < lines.size() - 1; i++) {
            String line = lines.get(i);
            StringBuilder sb = new StringBuilder();
            for (int j = start; j <= end; j++) {
                sb.append(j < line.length() ? line.charAt(j) : ' ');
            }
            
            // Extract the number by trimming spaces and removing any internal padding
            String cleanedNum = sb.toString().trim().replace(" ", "");
            if (!cleanedNum.isEmpty()) {
                try {
                    numbers.add(new BigInteger(cleanedNum));
                } catch (NumberFormatException ignored) {
                    // Skip strings that aren't valid numbers
                }
            }
        }

        // Return 0 if no numbers found in the block
        if (numbers.isEmpty()) return BigInteger.ZERO;

        // Perform the arithmetic operation across all operands in the vertical stack
        BigInteger result = numbers.get(0);
        for (int i = 1; i < numbers.size(); i++) {
            if (operator == '*') {
                result = result.multiply(numbers.get(i));
            } else {
                result = result.add(numbers.get(i));
            }
        }

        return result;
    }
}
