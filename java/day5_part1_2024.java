
import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Scanner;

public class Main {

    public static void main(String[] args) {
        List<int[]> orderingRules;
        List<int[]> updates;
        try {
            List<String> lines = readInput("input.txt");
            orderingRules = parseRules(lines);
            updates = parseUpdates(lines);

        } catch (FileNotFoundException e) {
            System.out.println("Error reading input: " + e.getMessage());
            return;
        }

        int sum = 0;
        for (int[] update : updates) {
            if (isCorrectlyOrdered(update, orderingRules)) {
                sum += update[update.length / 2];
            }
        }

        System.out.println(sum);
    }


    static List<String> readInput(String filename) throws FileNotFoundException {
        List<String> lines = new ArrayList<>();
        Scanner scanner = new Scanner(new File(filename));
        while (scanner.hasNextLine()) {
            lines.add(scanner.nextLine().trim());
        }
        scanner.close();
        return lines;
    }

    static List<int[]> parseRules(List<String> lines) {
        List<int[]> rules = new ArrayList<>();
        boolean ruleSection = true;
        for (String line : lines) {
            if (line.isEmpty()) {
                ruleSection = false;
                continue;
            }
            if (ruleSection) {
                String[] parts = line.split("\\|");
                if (parts.length == 2) {
                    try {
                        int x = Integer.parseInt(parts[0].trim());
                        int y = Integer.parseInt(parts[1].trim());
                        rules.add(new int[]{x, y});
                    } catch (NumberFormatException ignored) {}
                }
            } else {
                break;
            }
        }
        return rules;
    }

    static List<int[]> parseUpdates(List<String> lines) {
        List<int[]> updates = new ArrayList<>();
        boolean updateSection = false;
        for (String line : lines) {
            if (line.isEmpty()) {
                updateSection = true;
                continue;
            }
            if (updateSection) {
                String[] nums = line.split(",");
                int[] update = new int[nums.length];
                for (int i = 0; i < nums.length; i++) {
                    try {
                        update[i] = Integer.parseInt(nums[i].trim());
                    } catch (NumberFormatException ignored) {}
                }
                if (update.length > 0) {
                    updates.add(update);
                }
            }
        }
        return updates;
    }

    static boolean isCorrectlyOrdered(int[] update, List<int[]> rules) {
        Map<Integer, Integer> position = new HashMap<>();
        for (int i = 0; i < update.length; i++) {
            position.put(update[i], i);
        }

        for (int[] rule : rules) {
            int x = rule[0];
            int y = rule[1];
            if (position.containsKey(x) && position.containsKey(y)) {
                if (position.get(x) >= position.get(y)) {
                    return false;
                }
            }
        }
        return true;
    }
}
