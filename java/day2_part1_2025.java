
import java.io.IOException;
import java.math.BigInteger;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

/**
 * Solution for the Gift Shop Challenge.
 * The goal is to identify and sum all "invalid" product IDs within specific ranges.
 * An invalid ID is defined as a sequence of digits repeated exactly twice (e.g., 55, 6464, 123123).
 * Leading zeros are not allowed (e.g., 0101 is not a valid ID).
 */
public class Solution {

    public static void main(String[] args) {
        String fileName = "input.txt";
        Path path = Paths.get(fileName);

        if (!Files.exists(path)) {
            return;
        }

        try {
            // Read input file, remove all whitespace/newlines, and split by commas
            String content = Files.readString(path).replaceAll("\\s", "");
            if (content.isEmpty()) return;

            String[] rangeStrings = content.split(",");
            BigInteger totalInvalidSum = BigInteger.ZERO;

            for (String rangeStr : rangeStrings) {
                if (rangeStr.isEmpty()) continue;

                // Each range is provided in the format "firstID-lastID"
                String[] bounds = rangeStr.split("-");
                if (bounds.length != 2) continue;

                BigInteger low = new BigInteger(bounds[0]);
                BigInteger high = new BigInteger(bounds[1]);

                // Calculate the sum of invalid IDs within this specific range
                totalInvalidSum = totalInvalidSum.add(sumInvalidIdsInRange(low, high));
            }

            // Print the final result to standard output
            System.out.println(totalInvalidSum.toString());

        } catch (IOException | NumberFormatException e) {
            // Silently handle error or print for debugging
        }
    }

    /**
     * Efficiently calculates the sum of all "repeated sequence" IDs in the range [L, R].
     * 
     * Logic:
     * An invalid ID X has the form "SS", where S is a sequence of digits.
     * Mathematically: X = S * 10^k + S = S * (10^k + 1), where k is the number of digits in S.
     * Because leading zeros are disallowed, S must be in the range [10^(k-1), 10^k - 1].
     * 
     * We iterate through possible values of k and find the range of integers S 
     * that result in an invalid ID X within [L, R].
     */
    private static BigInteger sumInvalidIdsInRange(BigInteger L, BigInteger R) {
        BigInteger sum = BigInteger.ZERO;

        // k is the length of the repeated sequence S.
        // k=1 corresponds to 2-digit IDs, k=2 to 4-digit IDs, etc.
        // A limit of k=50 covers numbers up to 100 digits long.
        for (int k = 1; k <= 50; k++) {
            BigInteger powerOf10 = BigInteger.TEN.pow(k);
            BigInteger multiplier = powerOf10.add(BigInteger.ONE);

            // S must have exactly k digits to ensure X = SS has exactly 2k digits.
            BigInteger minSValue = BigInteger.TEN.pow(k - 1);
            BigInteger maxSValue = powerOf10.subtract(BigInteger.ONE);

            // We need to find all S such that: L <= S * multiplier <= R
            
            // sMin = ceiling(L / multiplier)
            BigInteger sMin = L.add(multiplier).subtract(BigInteger.ONE).divide(multiplier);
            // sMax = floor(R / multiplier)
            BigInteger sMax = R.divide(multiplier);

            // Find the intersection of [sMin, sMax] and the valid k-digit range [minSValue, maxSValue]
            BigInteger actualStart = sMin.max(minSValue);
            BigInteger actualEnd = sMax.min(maxSValue);

            if (actualStart.compareTo(actualEnd) <= 0) {
                // Number of valid S values in this range
                BigInteger count = actualEnd.subtract(actualStart).add(BigInteger.ONE);
                
                // Sum of an arithmetic progression for S: count * (first + last) / 2
                BigInteger sumOfSValues = count.multiply(actualStart.add(actualEnd)).divide(BigInteger.valueOf(2));
                
                // Add (sumOfSValues * multiplier) to the total range sum.
                // This is mathematically equivalent to summing all (S * (10^k + 1))
                sum = sum.add(sumOfSValues.multiply(multiplier));
            }
        }
        return sum;
    }
}
