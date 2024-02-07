
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

public class solution {
    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            String line;
            int[] adapters = new int[1000];
            int index = 0;
            while ((line = reader.readLine()) != null) {
                adapters[index++] = Integer.parseInt(line);
            }
            int[] sortedAdapters = Arrays.copyOf(adapters, index);
            Arrays.sort(sortedAdapters);

            Map<Integer, Integer> joltDifferences = new HashMap<>();
            joltDifferences.put(3, 1);
            int previousJoltage = 0;

            for (int adapter : sortedAdapters) {
                int diff = adapter - previousJoltage;
                joltDifferences.put(diff, joltDifferences.getOrDefault(diff, 0) + 1);
                previousJoltage = adapter;
            }

            int product = joltDifferences.get(1) * joltDifferences.get(3);
            System.out.println(product);

            reader.close();
        } catch (IOException e) {
            System.out.println("Error reading file: " + e);
        }
    }
}
