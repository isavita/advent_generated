
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

public class TowelArrangement {

    public static void main(String[] args) {
        try (BufferedReader reader = new BufferedReader(new FileReader("input.txt"))) {
            String patternsLine = reader.readLine();
            Set<String> patterns = new HashSet<>(Arrays.asList(patternsLine.split(", ")));
            reader.readLine(); // Skip the blank line

            int possibleDesigns = 0;
            String design;
            while ((design = reader.readLine()) != null) {
                if (isPossible(design, patterns)) {
                    possibleDesigns++;
                }
            }
            System.out.println(possibleDesigns);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private static boolean isPossible(String design, Set<String> patterns) {
        boolean[] dp = new boolean[design.length() + 1];
        dp[0] = true;

        for (int i = 1; i <= design.length(); i++) {
            for (int j = 1; j <= i; j++) {
                String sub = design.substring(i - j, i);
                if (patterns.contains(sub) && dp[i - j]) {
                    dp[i] = true;
                    break;
                }
            }
        }
        return dp[design.length()];
    }
}
