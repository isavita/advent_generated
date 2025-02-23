
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;

public class BridgeRepair {

    public static void main(String[] args) {
        long totalCalibrationResult = 0;

        try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
            String line;
            while ((line = br.readLine()) != null) {
                StringTokenizer colonTokenizer = new StringTokenizer(line, ":");
                long targetValue = Long.parseLong(colonTokenizer.nextToken().trim());
                String numbersString = colonTokenizer.nextToken().trim();

                StringTokenizer spaceTokenizer = new StringTokenizer(numbersString, " ");
                List<Long> numbers = new ArrayList<>();
                while (spaceTokenizer.hasMoreTokens()) {
                    numbers.add(Long.parseLong(spaceTokenizer.nextToken().trim()));
                }

                if (canReachTarget(numbers, targetValue)) {
                    totalCalibrationResult += targetValue;
                }
            }
        } catch (IOException e) {
            System.err.println("Error reading file: " + e.getMessage());
        }

        System.out.println(totalCalibrationResult);
    }

    private static boolean canReachTarget(List<Long> numbers, long targetValue) {
        return canReachTargetRecursive(numbers, 0, numbers.get(0), targetValue);
    }

    private static boolean canReachTargetRecursive(List<Long> numbers, int index, long currentResult, long targetValue) {
        if (index == numbers.size() - 1) {
            return currentResult == targetValue;
        }

        // Try adding
        if (canReachTargetRecursive(numbers, index + 1, currentResult + numbers.get(index + 1), targetValue)) {
            return true;
        }

        // Try multiplying
        if (canReachTargetRecursive(numbers, index + 1, currentResult * numbers.get(index + 1), targetValue)) {
            return true;
        }

        return false;
    }
}
