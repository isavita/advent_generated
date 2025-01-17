
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class TowelArrangement {

    public static void main(String[] args) {
        try (BufferedReader reader = new BufferedReader(new FileReader("input.txt"))) {
            String patternsLine = reader.readLine();
            String[] patterns = patternsLine.split(", ");
            Set<String> patternSet = new HashSet<>(Arrays.asList(patterns));

            reader.readLine(); // Skip the blank line

            String designLine;
            int possibleDesigns = 0;
            long totalArrangements = 0;
            while ((designLine = reader.readLine()) != null) {
                if (isPossible(designLine, patternSet)) {
                    possibleDesigns++;
                }
                totalArrangements += countArrangements(designLine, patternSet);
            }

            System.out.println("Part 1 - Possible designs: " + possibleDesigns);
            System.out.println("Part 2 - Total arrangements: " + totalArrangements);

        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private static boolean isPossible(String design, Set<String> patterns) {
        return countArrangements(design, patterns) > 0;
    }

    private static long countArrangements(String design, Set<String> patterns) {
        long[] dp = new long[design.length() + 1];
        dp[0] = 1;

        for (int i = 1; i <= design.length(); i++) {
            for (int j = 1; j <= i; j++) {
                String sub = design.substring(i - j, i);
                if (patterns.contains(sub)) {
                    dp[i] += dp[i - j];
                }
            }
        }

        return dp[design.length()];
    }
}
