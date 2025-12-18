
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

/**
 * Solution for Day 3: Lobby (Part Two).
 * 
 * The challenge asks us to find the largest 12-digit number that can be formed
 * by selecting exactly 12 digits (batteries) from each line (bank) while
 * preserving their original relative order. The final answer is the sum of
 * these maximum values from every bank.
 */
public class Lobby {

    public static void main(String[] args) {
        String fileName = "input.txt";
        long totalOutputJoltage = 0;

        try (BufferedReader reader = new BufferedReader(new FileReader(fileName))) {
            String line;
            while ((line = reader.readLine()) != null) {
                line = line.trim();
                if (line.isEmpty()) {
                    continue;
                }

                // Problem Part Two requires selecting exactly 12 digits to form the largest number.
                String largestSubsequence = findLargestSubsequence(line, 12);
                
                // Convert the resulting 12-digit string to a long and add to the total sum.
                // A 12-digit number (max 999,999,999,999) fits comfortably within a 64-bit long.
                totalOutputJoltage += Long.parseLong(largestSubsequence);
            }

            // Print the final result to standard output.
            System.out.println(totalOutputJoltage);

        } catch (IOException e) {
            System.err.println("Error reading file: " + e.getMessage());
        } catch (NumberFormatException e) {
            System.err.println("Error processing numeric data: " + e.getMessage());
        }
    }

    /**
     * Finds the largest subsequence of length k from the given string of digits.
     * 
     * Algorithm: Monotonic Stack
     * To maximize the number, we want the most significant digits (leftmost) 
     * to be as large as possible. We iterate through the string and maintain
     * a "stack" of digits. If we encounter a digit larger than the top of 
     * the stack, and we still have a "removal budget" (characters we can 
     * afford to skip while still reaching length k), we pop the stack.
     *
     * Complexity: O(N) time and O(N) space, where N is the length of the input string.
     *
     * @param s The input string of battery joltage digits.
     * @param k The number of digits to select (12 for this challenge).
     * @return The largest k-digit number as a string.
     */
    private static String findLargestSubsequence(String s, int k) {
        int n = s.length();
        int toRemove = n - k;
        
        // Use a char array to simulate a stack for efficiency.
        char[] stack = new char[n];
        int top = -1;

        for (int i = 0; i < n; i++) {
            char currentDigit = s.charAt(i);
            
            // While we have a removal budget AND the current digit is greater 
            // than the previous digit in our selection, discard the previous digit.
            while (toRemove > 0 && top >= 0 && stack[top] < currentDigit) {
                top--;
                toRemove--;
            }
            
            stack[++top] = currentDigit;
        }

        // The stack may contain more than k characters if toRemove reached 0 early.
        // The first k characters represent the optimal largest number.
        return new String(stack, 0, k);
    }
}
