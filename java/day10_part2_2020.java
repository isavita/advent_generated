
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
            int[] adapters = new int[1];
            adapters[0] = 0;
            int index = 1;

            while ((line = reader.readLine()) != null) {
                int joltage = Integer.parseInt(line);
                adapters = Arrays.copyOf(adapters, adapters.length + 1);
                adapters[index++] = joltage;
            }

            Arrays.sort(adapters);
            int[] extendedAdapters = Arrays.copyOf(adapters, adapters.length + 1);
            extendedAdapters[extendedAdapters.length - 1] = adapters[adapters.length - 1] + 3;

            System.out.println(countArrangements(extendedAdapters));
            reader.close();
        } catch (IOException e) {
            System.out.println("Error reading file: " + e);
        }
    }

    public static long countArrangements(int[] adapters) {
        Map<Integer, Long> ways = new HashMap<>();
        ways.put(0, 1L);

        for (int i = 1; i < adapters.length; i++) {
            int currentJoltage = adapters[i];
            for (int diff : new int[]{1, 2, 3}) {
                ways.putIfAbsent(currentJoltage, 0L);
                ways.put(currentJoltage, ways.get(currentJoltage) + ways.getOrDefault(currentJoltage - diff, 0L));
            }
        }

        return ways.get(adapters[adapters.length - 1]);
    }
}
