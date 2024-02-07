
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class solution {
    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            Map<Integer, String> rules = readRules(reader);
            String pattern = constructPattern(rules, 0);
            int count = countMatches(reader, pattern);
            System.out.println("The number of messages that completely match rule 0 is: " + count);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private static Map<Integer, String> readRules(BufferedReader reader) throws IOException {
        Map<Integer, String> rules = new HashMap<>();
        String line;
        while ((line = reader.readLine()) != null && !line.isEmpty()) {
            String[] parts = line.split(": ");
            rules.put(Integer.parseInt(parts[0]), parts[1].replace("\"", ""));
        }
        return rules;
    }

    private static String constructPattern(Map<Integer, String> rules, int index) {
        if (rules.get(index).contains("|")) {
            String[] subrules = rules.get(index).split(" \\| ");
            StringBuilder pattern = new StringBuilder();
            for (String subrule : subrules) {
                pattern.append(constructSubPattern(rules, subrule));
                pattern.append("|");
            }
            return "(" + pattern.substring(0, pattern.length() - 1) + ")";
        }
        return constructSubPattern(rules, rules.get(index));
    }

    private static String constructSubPattern(Map<Integer, String> rules, String subrule) {
        if (subrule.equals("a") || subrule.equals("b")) {
            return subrule;
        }
        String[] subIdxs = subrule.split(" ");
        StringBuilder pattern = new StringBuilder();
        for (String idx : subIdxs) {
            pattern.append(constructPattern(rules, Integer.parseInt(idx)));
        }
        return pattern.toString();
    }

    private static int countMatches(BufferedReader reader, String pattern) throws IOException {
        int count = 0;
        Pattern regex = Pattern.compile("^" + pattern + "$");
        String message;
        while ((message = reader.readLine()) != null) {
            Matcher matcher = regex.matcher(message);
            if (matcher.matches()) {
                count++;
            }
        }
        return count;
    }
}
