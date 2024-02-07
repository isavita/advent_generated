
import java.io.File;
import java.io.FileNotFoundException;
import java.util.HashMap;
import java.util.Map;
import java.util.Scanner;

public class solution {
    public static void main(String[] args) {
        try {
            File file = new File("input.txt");
            Scanner scanner = new Scanner(file);
            String initialState = "";
            Map<String, Character> rules = new HashMap<>();

            while (scanner.hasNextLine()) {
                String line = scanner.nextLine();
                if (line.contains("initial state")) {
                    initialState = line.split(": ")[1];
                } else if (line.contains("=>")) {
                    String[] parts = line.split(" => ");
                    rules.put(parts[0], parts[1].charAt(0));
                }
            }

            Map<Integer, Character> state = new HashMap<>();
            for (int i = 0; i < initialState.length(); i++) {
                if (initialState.charAt(i) == '#') {
                    state.put(i, '#');
                }
            }

            for (int generation = 0; generation < 20; generation++) {
                Map<Integer, Character> newState = new HashMap<>();
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
            }

            int sum = 0;
            for (int k : state.keySet()) {
                sum += k;
            }

            System.out.println(sum);
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    public static int[] minMaxKeys(Map<Integer, Character> m) {
        int minKey = Integer.MAX_VALUE;
        int maxKey = Integer.MIN_VALUE;
        for (int k : m.keySet()) {
            if (k < minKey) {
                minKey = k;
            }
            if (k > maxKey) {
                maxKey = k;
            }
        }
        return new int[]{minKey, maxKey};
    }
}
