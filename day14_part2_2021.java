
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

public class solution {
    public static void main(String[] args) {
        String[] input = readInput("input.txt");
        String template = input[0];
        Map<String, String> rules = new HashMap<>();
        for (int i = 1; i < input.length; i++) {
            String line = input[i];
            if (!line.isEmpty()) {
                String[] parts = line.split(" -> ");
                rules.put(parts[0], parts[1]);
            }
        }

        Map<String, Long> pairCounts = new HashMap<>();
        for (int i = 0; i < template.length() - 1; i++) {
            String pair = template.substring(i, i + 2);
            pairCounts.put(pair, pairCounts.getOrDefault(pair, 0L) + 1);
        }

        for (int step = 0; step < 40; step++) {
            Map<String, Long> newPairCounts = new HashMap<>();
            for (Map.Entry<String, Long> entry : pairCounts.entrySet()) {
                String pair = entry.getKey();
                long count = entry.getValue();
                if (rules.containsKey(pair)) {
                    String insert = rules.get(pair);
                    newPairCounts.put(pair.charAt(0) + insert, newPairCounts.getOrDefault(pair.charAt(0) + insert, 0L) + count);
                    newPairCounts.put(insert + pair.charAt(1), newPairCounts.getOrDefault(insert + pair.charAt(1), 0L) + count);
                } else {
                    newPairCounts.put(pair, newPairCounts.getOrDefault(pair, 0L) + count);
                }
            }
            pairCounts = newPairCounts;
        }

        Map<Character, Long> elementCounts = new HashMap<>();
        for (Map.Entry<String, Long> entry : pairCounts.entrySet()) {
            String pair = entry.getKey();
            long count = entry.getValue();
            elementCounts.put(pair.charAt(0), elementCounts.getOrDefault(pair.charAt(0), 0L) + count);
        }
        elementCounts.put(template.charAt(template.length() - 1), elementCounts.getOrDefault(template.charAt(template.length() - 1), 0L) + 1);

        long maxCount = 0, minCount = Long.MAX_VALUE;
        for (long count : elementCounts.values()) {
            maxCount = Math.max(maxCount, count);
            minCount = Math.min(minCount, count);
        }

        System.out.println(maxCount - minCount);
    }

    public static String[] readInput(String filename) {
        try (BufferedReader br = new BufferedReader(new FileReader(filename))) {
            return br.lines().toArray(String[]::new);
        } catch (IOException e) {
            e.printStackTrace();
            return new String[0];
        }
    }
}
