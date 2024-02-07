
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;

public class solution {
    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));

            String initialState = "";
            HashMap<String, Character> rules = new HashMap<>();

            String line;
            while ((line = reader.readLine()) != null) {
                if (line.contains("initial state")) {
                    initialState = line.split(": ")[1];
                } else if (line.contains("=>")) {
                    String[] parts = line.split(" => ");
                    rules.put(parts[0], parts[1].charAt(0));
                }
            }

            HashMap<Integer, Character> state = new HashMap<>();
            for (int i = 0; i < initialState.length(); i++) {
                if (initialState.charAt(i) == '#') {
                    state.put(i, '#');
                }
            }

            String previousPattern = "";
            int previousSum = 0;
            int offset = 0;
            for (int generation = 0; generation < 50000000000L; generation++) {
                HashMap<Integer, Character> newState = new HashMap<>();
                int[] minMax = minMaxKeys(state);
                int minPot = minMax[0];
                int maxPot = minMax[1];
                for (int i = minPot - 2; i <= maxPot + 2; i++) {
                    StringBuilder pattern = new StringBuilder();
                    for (int j = i - 2; j <= i + 2; j++) {
                        if (state.containsKey(j) && state.get(j) == '#') {
                            pattern.append("#");
                        } else {
                            pattern.append(".");
                        }
                    }
                    if (rules.containsKey(pattern.toString()) && rules.get(pattern.toString()) == '#') {
                        newState.put(i, '#');
                    }
                }
                state = newState;

                String[] result = statePattern(state);
                String currentPattern = result[0];
                int currentSum = Integer.parseInt(result[1]);
                if (currentPattern.equals(previousPattern)) {
                    offset = currentSum - previousSum;
                    long remainingGenerations = 50000000000L - generation - 1;
                    long finalSum = currentSum + offset * remainingGenerations;
                    System.out.println(finalSum);
                    return;
                }
                previousPattern = currentPattern;
                previousSum = currentSum;
            }

            reader.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public static int[] minMaxKeys(HashMap<Integer, Character> map) {
        int minKey = Integer.MAX_VALUE;
        int maxKey = Integer.MIN_VALUE;
        for (int key : map.keySet()) {
            if (key < minKey) {
                minKey = key;
            }
            if (key > maxKey) {
                maxKey = key;
            }
        }
        return new int[]{minKey, maxKey};
    }

    public static String[] statePattern(HashMap<Integer, Character> map) {
        int minPot = minMaxKeys(map)[0];
        int maxPot = minMaxKeys(map)[1];
        StringBuilder pattern = new StringBuilder();
        int sum = 0;
        for (int i = minPot; i <= maxPot; i++) {
            if (map.containsKey(i) && map.get(i) == '#') {
                pattern.append("#");
                sum += i;
            } else {
                pattern.append(".");
            }
        }
        return new String[]{pattern.toString(), Integer.toString(sum)};
    }
}
