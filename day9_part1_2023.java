
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class solution {
    public static void main(String[] args) {
        List<String> input = readFile("input.txt");
        System.out.println(solve(input));
    }

    private static List<String> readFile(String fileName) {
        List<String> input = new ArrayList<>();
        try (BufferedReader br = new BufferedReader(new FileReader(fileName))) {
            String line;
            while ((line = br.readLine()) != null) {
                input.add(line);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        return input;
    }

    private static List<List<Integer>> parseInput(List<String> input) {
        List<List<Integer>> histories = new ArrayList<>();
        for (String line : input) {
            histories.add(parseStringToInts(line));
        }
        return histories;
    }

    private static List<Integer> parseStringToInts(String numbersLine) {
        List<Integer> numbers = new ArrayList<>();
        String[] numbersParts = numbersLine.split(" ");
        for (String numberStr : numbersParts) {
            numbers.add(Integer.parseInt(numberStr));
        }
        return numbers;
    }

    private static boolean allZeros(List<Integer> nums) {
        for (int num : nums) {
            if (num != 0) {
                return false;
            }
        }
        return true;
    }

    private static List<Integer> calculateExtrapolation(List<Integer> history) {
        List<Integer> extrapolations = new ArrayList<>();
        for (int i = 1; i < history.size(); i++) {
            int extrapolation = history.get(i) - history.get(i - 1);
            extrapolations.add(extrapolation);
        }
        return extrapolations;
    }

    private static List<List<Integer>> calculateExtrapolations(List<Integer> history) {
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

    private static int solve(List<String> input) {
        List<List<Integer>> histories = parseInput(input);
        int res = 0;

        for (List<Integer> history : histories) {
            List<List<Integer>> extrapolationsSeries = calculateExtrapolations(history);

            int futurePrediction = 0;
            for (int i = extrapolationsSeries.size() - 1; i > -1; i--) {
                futurePrediction = extrapolationsSeries.get(i).get(extrapolationsSeries.get(i).size() - 1) + futurePrediction;
            }

            res += futurePrediction;
        }

        return res;
    }
}
