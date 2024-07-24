
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;

public class Main {
    private static final int HASH_TABLE_SIZE = 256;

    static class Step {
        String label;
        int numBox;
        String operation;
        int number;

        Step(String label, int numBox, String operation, int number) {
            this.label = label;
            this.numBox = numBox;
            this.operation = operation;
            this.number = number;
        }
    }

    private static int hashString(String str) {
        int res = 0;
        for (char c : str.toCharArray()) {
            res = (res + c) * 17 % HASH_TABLE_SIZE;
        }
        return res;
    }

    private static Step parseStep(String stepStr) {
        String label = stepStr.replaceAll("[-=\\d]", "");
        int numBox = hashString(label);
        String operation = stepStr.substring(label.length(), label.length() + 1);
        int number = operation.equals("=") ? Integer.parseInt(stepStr.substring(label.length() + 1)) : 0;

        return new Step(label, numBox, operation, number);
    }

    private static Map<Integer, List<Map<String, Integer>>> getBoxes(String[] stepsStr) {
        Map<Integer, List<Map<String, Integer>>> boxes = new HashMap<>();

        for (String stepStr : stepsStr) {
            Step step = parseStep(stepStr);
            List<Map<String, Integer>> boxContents = boxes.computeIfAbsent(step.numBox, k -> new ArrayList<>());

            if ("-".equals(step.operation)) {
                boxContents.removeIf(content -> content.containsKey(step.label));
            } else if ("=".equals(step.operation)) {
                boolean found = false;
                for (Map<String, Integer> content : boxContents) {
                    if (content.containsKey(step.label)) {
                        content.put(step.label, step.number);
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    boxContents.add(new HashMap<>(Map.of(step.label, step.number)));
                }
            }
            if (boxContents.isEmpty()) {
                boxes.remove(step.numBox);
            }
        }

        return boxes;
    }

    private static int calculatePower(Map<Integer, List<Map<String, Integer>>> boxes) {
        int res = 0;

        for (int iBox = 0; iBox < HASH_TABLE_SIZE; iBox++) {
            List<Map<String, Integer>> boxContents = boxes.get(iBox);
            if (boxContents != null) {
                for (int iSlot = 0; iSlot < boxContents.size(); iSlot++) {
                    for (int value : boxContents.get(iSlot).values()) {
                        res += (iBox + 1) * (iSlot + 1) * value;
                    }
                }
            }
        }

        return res;
    }

    private static int solve(String input) {
        String[] stepsStr = input.split(",");
        Map<Integer, List<Map<String, Integer>>> boxes = getBoxes(stepsStr);
        return calculatePower(boxes);
    }

    private static String readFile(String fileName) throws IOException {
        return new String(Files.readAllBytes(Paths.get(fileName))).trim();
    }

    public static void main(String[] args) throws IOException {
        String input = readFile("input.txt");
        System.out.println(solve(input));
    }
}
