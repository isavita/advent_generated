
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Arrays;
import java.util.Scanner;

public class SafeReport {

    public static void main(String[] args) {
        try (Scanner scanner = new Scanner(new File("input.txt"))) {
            int safeReportCount = 0;
            while (scanner.hasNextLine()) {
                int[] levels = parseLevels(scanner.nextLine());
                if (isSafeReport(levels) || isSafeWithOneRemoval(levels)) {
                    safeReportCount++;
                }
            }
            System.out.println(safeReportCount);
        } catch (FileNotFoundException e) {
            System.err.println("Error opening file: " + e.getMessage());
        }
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
        boolean increasing = firstDiff > 0;
        for (int i = 0; i < levels.length - 1; i++) {
            int diff = levels[i + 1] - levels[i];
            if (diff == 0 || (increasing && diff <= 0) || (!increasing && diff >= 0) || Math.abs(diff) < 1 || Math.abs(diff) > 3)
                return false;
        }
        return true;
    }

    static boolean isSafeWithOneRemoval(int[] levels) {
        for (int i = 0; i < levels.length; i++) {
            int[] modifiedLevels = new int[levels.length - 1];
            System.arraycopy(levels, 0, modifiedLevels, 0, i);
            System.arraycopy(levels, i + 1, modifiedLevels, i, levels.length - i - 1);
            if (isSafeReport(modifiedLevels)) return true;
        }
        return false;
    }
}
