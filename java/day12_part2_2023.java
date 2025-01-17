
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

public class HotSprings {

    public static void main(String[] args) {
        long totalArrangements = 0;
        long totalArrangementsUnfolded = 0;

        try (BufferedReader reader = new BufferedReader(new FileReader("input.txt"))) {
            String line;
            while ((line = reader.readLine()) != null) {
                String[] parts = line.split(" ");
                String springs = parts[0];
                int[] groups = Arrays.stream(parts[1].split(",")).mapToInt(Integer::parseInt).toArray();

                totalArrangements += countArrangements(springs, groups, new HashMap<>());
                totalArrangementsUnfolded += countArrangementsUnfolded(springs, groups, new HashMap<>());
            }
        } catch (IOException e) {
            e.printStackTrace();
        }

        System.out.println("Total arrangements: " + totalArrangements);
        System.out.println("Total arrangements (unfolded): " + totalArrangementsUnfolded);
    }

    private static long countArrangements(String springs, int[] groups, Map<String, Long> memo) {
        String key = springs + Arrays.toString(groups);
        if (memo.containsKey(key)) {
            return memo.get(key);
        }

        if (springs.isEmpty()) {
            return groups.length == 0 ? 1 : 0;
        }

        if (groups.length == 0) {
            return springs.contains("#") ? 0 : 1;
        }

        long result = 0;

        if (springs.charAt(0) == '.' || springs.charAt(0) == '?') {
            result += countArrangements(springs.substring(1), groups, memo);
        }

        if (springs.charAt(0) == '#' || springs.charAt(0) == '?') {
            int groupSize = groups[0];
            if (groupSize <= springs.length() && !springs.substring(0, groupSize).contains(".") &&
                (groupSize == springs.length() || springs.charAt(groupSize) != '#')) {
                int[] newGroups = Arrays.copyOfRange(groups, 1, groups.length);
                String newSprings = groupSize + 1 <= springs.length() ? springs.substring(groupSize + 1) : "";
                result += countArrangements(newSprings, newGroups, memo);
            }
        }

        memo.put(key, result);
        return result;
    }

    private static long countArrangementsUnfolded(String springs, int[] groups, Map<String, Long> memo) {
        String unfoldedSprings = String.join("?", springs, springs, springs, springs, springs);
        int[] unfoldedGroups = new int[groups.length * 5];
        for (int i = 0; i < 5; i++) {
            System.arraycopy(groups, 0, unfoldedGroups, i * groups.length, groups.length);
        }
        return countArrangements(unfoldedSprings, unfoldedGroups, memo);
    }
}
