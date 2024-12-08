
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Arrays;
import java.util.Scanner;

public class SafeReportCounter {

    public static void main(String[] args) {
        int safeReportCount = 0;
        try (Scanner scanner = new Scanner(new File("input.txt"))) {
            while (scanner.hasNextLine()) {
                String line = scanner.nextLine();
                int[] levels = parseLevels(line);
                if (isSafeReport(levels)) {
                    safeReportCount++;
                }
            }
        } catch (FileNotFoundException e) {
            System.err.println("Error opening file: " + e.getMessage());
            return;
        }
        System.out.println(safeReportCount);
    }

    static int[] parseLevels(String line) {
        return Arrays.stream(line.split("\\s+"))
                .mapToInt(Integer::parseInt)
                .toArray();
    }

    static boolean isSafeReport(int[] levels) {
        if (levels.length < 2) return false;
        int firstDiff = levels[1] - levels[0];
        if (firstDiff == 0) return false;
        boolean isIncreasing = firstDiff > 0;
        for (int i = 0; i < levels.length - 1; i++) {
            int diff = levels[i + 1] - levels[i];
            if (diff == 0 || (isIncreasing && diff <= 0) || (!isIncreasing && diff >= 0) || Math.abs(diff) < 1 || Math.abs(diff) > 3)
                return false;
        }
        return true;
    }
}
