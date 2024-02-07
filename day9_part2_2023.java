
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

public class solution {
    public static List<List<Integer>> parseInput(List<String> input) {
        List<List<Integer>> histories = new ArrayList<>();
        for (String line : input) {
            List<Integer> numbers = parseStringToInts(line);
            histories.add(numbers);
        }
        return histories;
    }

    public static List<Integer> parseStringToInts(String numbersLine) {
        List<Integer> numbers = new ArrayList<>();
        String[] numbersParts = numbersLine.split(" ");
        for (String numberStr : numbersParts) {
            int number = Integer.parseInt(numberStr);
            numbers.add(number);
        }
        return numbers;
    }

    public static boolean allZeros(List<Integer> nums) {
        for (int num : nums) {
            if (num != 0) {
                return false;
            }
        }
        return true;
    }

    public static List<Integer> calculateExtrapolation(List<Integer> history) {
        List<Integer> extrapolations = new ArrayList<>();
        for (int i = 1; i < history.size(); i++) {
            int extrapolation = history.get(i) - history.get(i - 1);
            extrapolations.add(extrapolation);
        }
        return extrapolations;
    }

    public static List<List<Integer>> calculateExtrapolations(List<Integer> history) {
        List<List<Integer>> extrapolationsSeries = new ArrayList<>();
        extrapolationsSeries.add(history);

        for (int i = 1; i < history.size(); i++) {
            List<Integer> previousExtrapolations = extrapolationsSeries.get(i - 1);
            if (allZeros(previousExtrapolations)) {
                return extrapolationsSeries;
            }

            List<Integer> extrapolations = calculateExtrapolation(previousExtrapolations);
            extrapolationsSeries.add(extrapolations);
        }

        return extrapolationsSeries;
    }

    public static int solve(List<String> input) {
        List<List<Integer>> histories = parseInput(input);
        int res = 0;

        for (List<Integer> history : histories) {
            List<List<Integer>> extrapolationsSeries = calculateExtrapolations(history);

            int pastPrediction = 0;
            for (int i = extrapolationsSeries.size() - 1; i > -1; i--) {
                pastPrediction = extrapolationsSeries.get(i).get(0) - pastPrediction;
            }

            res += pastPrediction;
        }

        return res;
    }

    public static List<String> readFile(String fileName) throws IOException {
        return Files.readAllLines(Paths.get(fileName));
    }

    public static void main(String[] args) throws IOException {
        List<String> input = readFile("input.txt");
        System.out.println(solve(input));
    }
}
