
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

public class Solution {
    private static final int hashTableSize = 256;

    static class Step {
        String label;
        int numBox;
        String operation;
        int number;
    }

    private static int hashString(String str) {
        int res = 0;
        for (int i = 0; i < str.length(); i++) {
            char c = str.charAt(i);
            res += (int) c;
            res *= 17;
            res %= hashTableSize;
        }
        return res;
    }

    private static Step parseStep(String stepStr) {
        Step step = new Step();

        step.label = stepStr.replaceAll("=|-|\\d", "");
        step.numBox = hashString(step.label);
        step.operation = stepStr.substring(step.label.length(), step.label.length()+1);
        if (step.operation.equals("=")) {
            step.number = Integer.parseInt(stepStr.substring(step.label.length()+1));
        }

        return step;
    }

    private static int solve(List<String> input) {
        String line = input.get(0);
        String[] steps = line.split(",");
        int res = 0;
        for (String step : steps) {
            res += hashString(step);
        }
        return res;
    }

    private static List<String> readFile(String fileName) throws IOException {
        return Files.readAllLines(Paths.get(fileName));
    }

    public static void main(String[] args) {
        try {
            List<String> input = readFile("input.txt");
            System.out.println(solve(input));
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
