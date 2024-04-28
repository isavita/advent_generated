import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

public class Main {
    public static void main(String[] args) throws IOException {
        BufferedReader br = new BufferedReader(new FileReader("input.txt"));
        String[] strArr = br.readLine().trim().split("\\s+");
        int[] banks = new int[strArr.length];
        for (int i = 0; i < strArr.length; i++) {
            banks[i] = Integer.parseInt(strArr[i]);
        }

        Map<String, Boolean> seen = new HashMap<>();
        int cycles = 0;

        while (true) {
            String state = java.util.Arrays.toString(banks);

            if (seen.containsKey(state)) {
                break;
            }
            seen.put(state, true);

            int maxIndex = 0;
            for (int i = 1; i < banks.length; i++) {
                if (banks[i] > banks[maxIndex]) {
                    maxIndex = i;
                }
            }

            int blocks = banks[maxIndex];
            banks[maxIndex] = 0;
            for (int i = 1; i <= blocks; i++) {
                banks[(maxIndex + i) % banks.length]++;
            }

            cycles++;
        }

        System.out.println("It takes " + cycles + " redistribution cycles to reach a repeated configuration.");
    }
}