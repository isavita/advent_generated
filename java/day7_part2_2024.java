
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class BridgeRepair {

    public static void main(String[] args) {
        String inputFile = "input.txt";
        long totalCalibrationResult = 0;
        long totalCalibrationResultPart2 = 0;

        try (BufferedReader br = new BufferedReader(new FileReader(inputFile))) {
            String line;
            while ((line = br.readLine()) != null) {
                String[] parts = line.split(":");
                long target = Long.parseLong(parts[0].trim());
                String[] numbersStr = parts[1].trim().split(" ");
                List<Long> numbers = new ArrayList<>();
                for (String numStr : numbersStr) {
                    numbers.add(Long.parseLong(numStr));
                }

                if (canBeTrue(numbers, target, false)) {
                    totalCalibrationResult += target;
                }
                if (canBeTrue(numbers, target, true)) {
                    totalCalibrationResultPart2 += target;
                }
            }
        } catch (IOException e) {
            System.err.println("Error reading file: " + e.getMessage());
            return;
        }

        System.out.println("Total calibration result (Part 1): " + totalCalibrationResult);
        System.out.println("Total calibration result (Part 2): " + totalCalibrationResultPart2);
    }

    private static boolean canBeTrue(List<Long> numbers, long target, boolean allowConcat) {
        return solve(numbers, 0, numbers.get(0), target, allowConcat);
    }

    private static boolean solve(List<Long> numbers, int index, long currentResult, long target, boolean allowConcat) {
        if (index == numbers.size() - 1) {
            return currentResult == target;
        }

        int nextIndex = index + 1;
        long nextNumber = numbers.get(nextIndex);

        if (solve(numbers, nextIndex, currentResult + nextNumber, target, allowConcat)) {
            return true;
        }
        if (solve(numbers, nextIndex, currentResult * nextNumber, target, allowConcat)) {
            return true;
        }

        if (allowConcat) {
            String concatResult = String.valueOf(currentResult) + String.valueOf(nextNumber);
            try
            {
                long concatNum = Long.parseLong(concatResult);
                 if (solve(numbers, nextIndex, concatNum, target, allowConcat))
                 {
                    return true;
                 }

            }
            catch(NumberFormatException e)
            {
                //ignore large numbers.
            }


        }

        return false;
    }
}
