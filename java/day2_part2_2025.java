
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.HashSet;
import java.util.Set;

/**
 * Solution for "Day 2: Gift Shop".
 * 
 * This program identifies "invalid" product IDs within specified ranges.
 * According to the puzzle:
 * - Part 1: An ID is invalid if it consists of a sequence of digits repeated exactly twice.
 * - Part 2: An ID is invalid if it consists of a sequence of digits repeated at least twice.
 * 
 * The program reads ID ranges from 'input.txt', iterates through the ranges,
 * identifies unique invalid IDs according to the Part 2 rules, and prints their sum.
 */
public class GiftShop {

    public static void main(String[] args) {
        String input;
        try {
            // Read entire input from 'input.txt'
            input = new String(Files.readAllBytes(Paths.get("input.txt")));
        } catch (IOException e) {
            // If file reading fails, terminate silently or log an error
            return;
        }

        // Clean input: Remove whitespace (newlines, tabs, spaces)
        String cleanedInput = input.replaceAll("\\s+", "");
        if (cleanedInput.isEmpty()) return;

        // The input contains comma-separated ranges (e.g., "11-22,95-115")
        String[] ranges = cleanedInput.split(",");
        
        // Use a Set to ensure each invalid ID is counted only once, even if ranges overlap
        Set<Long> uniqueInvalidIds = new HashSet<>();

        for (String range : ranges) {
            if (range.isEmpty()) continue;
            
            // Each range is "start-end"
            String[] parts = range.split("-");
            if (parts.length != 2) continue;

            try {
                long start = Long.parseLong(parts[0]);
                long end = Long.parseLong(parts[1]);

                // Check every ID within the current range
                for (long currentId = start; currentId <= end; currentId++) {
                    if (isInvalid(currentId)) {
                        uniqueInvalidIds.add(currentId);
                    }
                }
            } catch (NumberFormatException e) {
                // Ignore malformed range segments
            }
        }

        // Sum up all unique invalid IDs found
        long totalSum = 0;
        for (long id : uniqueInvalidIds) {
            totalSum += id;
        }

        // Print final result to standard output
        System.out.println(totalSum);
    }

    /**
     * Determines if a product ID is invalid based on Part 2 criteria:
     * The sequence of digits must repeat at least twice.
     * 
     * @param n The ID to evaluate.
     * @return true if the ID is invalid (repeating pattern), false otherwise.
     */
    private static boolean isInvalid(long n) {
        String s = Long.toString(n);
        int totalLength = s.length();
        
        // A sequence of length 'totalLength' is invalid if it has a period 'd' 
        // such that d divides totalLength and the string is composed of 
        // that period repeated at least twice (i.e., totalLength / d >= 2).
        for (int d = 1; d <= totalLength / 2; d++) {
            if (totalLength % d == 0) {
                if (isRepeatingPattern(s, d)) {
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * Checks if a string consists entirely of a repeating pattern of length d.
     * 
     * @param s The string to check.
     * @param d The length of the pattern prefix.
     * @return true if the prefix of length d repeats throughout the entire string.
     */
    private static boolean isRepeatingPattern(String s, int d) {
        int totalLength = s.length();
        // Compare the first block of length 'd' against all subsequent blocks
        for (int i = d; i < totalLength; i += d) {
            for (int j = 0; j < d; j++) {
                if (s.charAt(j) != s.charAt(i + j)) {
                    return false;
                }
            }
        }
        return true;
    }
}
