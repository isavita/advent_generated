
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class solution {
    static class BagRule {
        String color;
        int count;

        BagRule(String color, int count) {
            this.color = color;
            this.count = count;
        }
    }

    public static void main(String[] args) {
        Map<String, BagRule[]> rules = new HashMap<>();
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            String line;
            Pattern rulePattern = Pattern.compile("(\\d+) (\\w+ \\w+) bags?[,.]");
            while ((line = reader.readLine()) != null) {
                String[] parts = line.split(" bags contain ");
                String container = parts[0];
                String contents = parts[1];

                if (contents.equals("no other bags.")) {
                    continue;
                }

                Matcher matcher = rulePattern.matcher(contents);
                while (matcher.find()) {
                    int count = Integer.parseInt(matcher.group(1));
                    BagRule rule = new BagRule(matcher.group(2), count);
                    if (!rules.containsKey(container)) {
                        rules.put(container, new BagRule[]{rule});
                    } else {
                        BagRule[] existingRules = rules.get(container);
                        BagRule[] newRules = new BagRule[existingRules.length + 1];
                        System.arraycopy(existingRules, 0, newRules, 0, existingRules.length);
                        newRules[newRules.length - 1] = rule;
                        rules.put(container, newRules);
                    }
                }
            }
            reader.close();
        } catch (IOException e) {
            System.out.println("Error reading file: " + e.getMessage());
            return;
        }

        int totalBags = countBags("shiny gold", rules) - 1;
        System.out.println(totalBags);
    }

    public static int countBags(String color, Map<String, BagRule[]> rules) {
        int count = 1;
        if (rules.containsKey(color)) {
            for (BagRule rule : rules.get(color)) {
                count += rule.count * countBags(rule.color, rules);
            }
        }
        return count;
    }
}
