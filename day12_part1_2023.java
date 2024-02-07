
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class solution {
    static class Row {
        String springs;
        List<Integer> group;

        Row(String springs, List<Integer> group) {
            this.springs = springs;
            this.group = group;
        }
    }

    public static void main(String[] args) {
        List<String> input = readFile("input.txt");
        System.out.println(solve(input));
    }

    private static List<Row> parseInput(List<String> input) {
        List<Row> rows = new ArrayList<>();
        for (String line : input) {
            String[] parts = line.split(" ");
            String springs = parts[0];
            List<Integer> ints = parseStringToInts(parts[1]);

            Row row = new Row(springs, ints);
            rows.add(row);
        }
        return rows;
    }

    private static List<Integer> parseStringToInts(String numbersLine) {
        List<Integer> numbers = new ArrayList<>();
        String[] numbersParts = numbersLine.split(",");
        for (String numberStr : numbersParts) {
            int number = Integer.parseInt(numberStr);
            numbers.add(number);
        }
        return numbers;
    }

    private static int countArrangementsRecursive(Row row, int iSprings, int iGroup, int iContiguousDamaged, Map<List<Integer>, Integer> cache) {
        if (iSprings == row.springs.length()) {
            if (iGroup == row.group.size() && iContiguousDamaged == 0) {
                return 1;
            } else if (iGroup == row.group.size() - 1 && iContiguousDamaged == row.group.get(iGroup)) {
                return 1;
            }
            return 0;
        }

        List<Integer> cacheKey = List.of(iSprings, iGroup, iContiguousDamaged);
        if (cache.containsKey(cacheKey)) {
            return cache.get(cacheKey);
        }

        int res = 0;
        char ch = row.springs.charAt(iSprings);
        if (ch == '.' || ch == '?') {
            if (iContiguousDamaged == 0) {
                res += countArrangementsRecursive(row, iSprings + 1, iGroup, iContiguousDamaged, cache);
            } else if (iContiguousDamaged == row.group.get(iGroup)) {
                res += countArrangementsRecursive(row, iSprings + 1, iGroup + 1, 0, cache);
            }
        }
        if (ch == '#' || ch == '?') {
            if (iGroup < row.group.size() && iContiguousDamaged < row.group.get(iGroup)) {
                res += countArrangementsRecursive(row, iSprings + 1, iGroup, iContiguousDamaged + 1, cache);
            }
        }

        cache.put(cacheKey, res);
        return res;
    }

    private static int countArrangements(Row row) {
        return countArrangementsRecursive(row, 0, 0, 0, new HashMap<>());
    }

    private static Row unfoldRow(Row row, int unfoldingFactor) {
        Row newRow = new Row(row.springs, new ArrayList<>(row.group));

        for (int i = 1; i < unfoldingFactor; i++) {
            newRow.springs += "?" + row.springs;
            newRow.group.addAll(row.group);
        }

        return newRow;
    }

    private static int solve(List<String> input) {
        List<Row> rows = parseInput(input);

        int res = 0;
        for (Row row : rows) {
            res += countArrangements(row);
        }

        return res;
    }

    private static List<String> readFile(String fileName) {
        List<String> lines = new ArrayList<>();
        try (BufferedReader br = new BufferedReader(new FileReader(fileName))) {
            String line;
            while ((line = br.readLine()) != null) {
                lines.add(line);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        return lines;
    }
}
