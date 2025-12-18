
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class Lobby {
    public static void main(String[] args) {
        String fileName = "input.txt";
        long totalJoltage = 0;

        try (BufferedReader br = new BufferedReader(new FileReader(fileName))) {
            String line;
            while ((line = br.readLine()) != null) {
                line = line.trim();
                if (!line.isEmpty()) {
                    totalJoltage += findMaxJoltage(line);
                }
            }
            System.out.println(totalJoltage);
        } catch (IOException e) {
            System.err.println("Error reading input file: " + e.getMessage());
        }
    }

    /**
     * Finds the maximum joltage for a bank by picking two digits (T, U)
     * where T appears before U in the string.
     */
    private static int findMaxJoltage(String bank) {
        // To maximize 10*T + U, we iterate from the highest possible digit (9)
        // down to 1 to find the best candidate for the tens place.
        for (char t = '9'; t >= '1'; t--) {
            int firstIdx = bank.indexOf(t);

            // A valid tens digit must not be the last character in the bank
            if (firstIdx != -1 && firstIdx < bank.length() - 1) {
                int maxU = 0;
                
                // Once the highest possible tens digit is found, 
                // find the largest digit that exists after it.
                for (int j = firstIdx + 1; j < bank.length(); j++) {
                    int currentDigit = bank.charAt(j) - '0';
                    if (currentDigit > maxU) {
                        maxU = currentDigit;
                    }
                }
                
                // Since we chose the largest possible T that can be a tens digit,
                // and then the largest possible U after it, this is the maximum.
                return (t - '0') * 10 + maxU;
            }
        }
        return 0;
    }
}
