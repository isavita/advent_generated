
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

public class Solution {
    private static Map<String, String> memo = new HashMap<>();

    public static void main(String[] args) {
        Map<String, String> rules = new HashMap<>();

        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            String line;
            while ((line = reader.readLine()) != null) {
                String[] parts = line.split(" => ");
                rules.put(parts[0], parts[1]);
            }
            reader.close();
        } catch (IOException e) {
            e.printStackTrace();
        }

        String[] grid = {
            ".#.",
            "..#",
            "###"
        };

        for (int i = 0; i < 18; i++) {
            int newSize;
            int subSize;

            if (grid.length % 2 == 0) {
                subSize = 2;
                newSize = grid.length / 2 * 3;
            } else {
                subSize = 3;
                newSize = grid.length / 3 * 4;
            }

            String[] newGrid = new String[newSize];
            for (int x = 0; x < newSize; x++) {
                newGrid[x] = "";
            }

            for (int y = 0; y < grid.length; y += subSize) {
                for (int x = 0; x < grid.length; x += subSize) {
                    String[] square = new String[subSize];
                    for (int dy = 0; dy < subSize; dy++) {
                        square[dy] = grid[y + dy].substring(x, x + subSize);
                    }
                    String newSquare = enhance(String.join("/", square), rules);
                    String[] newRows = newSquare.split("/");
                    for (int dy = 0; dy < newRows.length; dy++) {
                        newGrid[y / subSize * (subSize + 1) + dy] += newRows[dy];
                    }
                }
            }
            grid = newGrid;
        }

        int count = 0;
        for (String row : grid) {
            for (char pixel : row.toCharArray()) {
                if (pixel == '#') {
                    count++;
                }
            }
        }
        System.out.println(count);
    }

    private static String enhance(String input, Map<String, String> rules) {
        if (memo.containsKey(input)) {
            return memo.get(input);
        }

        String original = input;
        for (int i = 0; i < 4; i++) {
            if (rules.containsKey(input)) {
                String output = rules.get(input);
                memo.put(original, output);
                return output;
            }
            input = rotate(input);
        }
        input = flip(input);
        for (int i = 0; i < 4; i++) {
            if (rules.containsKey(input)) {
                String output = rules.get(input);
                memo.put(original, output);
                return output;
            }
            input = rotate(input);
        }
        return "";
    }

    private static String rotate(String input) {
        String[] parts = input.split("/");
        int size = parts.length;
        String[] newParts = new String[size];
        for (int x = 0; x < size; x++) {
            StringBuilder newRow = new StringBuilder();
            for (int y = size - 1; y >= 0; y--) {
                newRow.append(parts[y].charAt(x));
            }
            newParts[x] = newRow.toString();
        }
        return String.join("/", newParts);
    }

    private static String flip(String input) {
        String[] parts = input.split("/");
        for (int i = 0; i < parts.length; i++) {
            parts[i] = new StringBuilder(parts[i]).reverse().toString();
        }
        return String.join("/", parts);
    }
}
