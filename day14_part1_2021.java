
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

public class solution {
    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            String polymer = reader.readLine();
            Map<String, String> rules = new HashMap<>();

            String line;
            while ((line = reader.readLine()) != null) {
                if (line.equals("")) {
                    continue;
                }
                String[] parts = line.split(" -> ");
                rules.put(parts[0], parts[1]);
            }

            for (int step = 0; step < 10; step++) {
                polymer = applyInsertion(polymer, rules);
            }

            Map<Character, Integer> counts = countElements(polymer);
            int[] minMax = minMax(counts);

            System.out.println(minMax[1] - minMax[0]);

            reader.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public static String applyInsertion(String polymer, Map<String, String> rules) {
        StringBuilder newPolymer = new StringBuilder();
        for (int i = 0; i < polymer.length() - 1; i++) {
            newPolymer.append(polymer.charAt(i));
            if (rules.containsKey(polymer.substring(i, i + 2))) {
                newPolymer.append(rules.get(polymer.substring(i, i + 2)));
            }
        }
        newPolymer.append(polymer.charAt(polymer.length() - 1));
        return newPolymer.toString();
    }

    public static Map<Character, Integer> countElements(String polymer) {
        Map<Character, Integer> counts = new HashMap<>();
        for (char c : polymer.toCharArray()) {
            counts.put(c, counts.getOrDefault(c, 0) + 1);
        }
        return counts;
    }

    public static int[] minMax(Map<Character, Integer> counts) {
        int min = Integer.MAX_VALUE;
        int max = Integer.MIN_VALUE;
        for (int count : counts.values()) {
            if (count < min) {
                min = count;
            }
            if (count > max) {
                max = count;
            }
        }
        return new int[]{min, max};
    }
}
