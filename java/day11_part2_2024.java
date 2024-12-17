
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.math.BigInteger;
import java.util.HashMap;
import java.util.Map;

public class Main {

    private static String trimLeadingZeros(String s) {
        int i = 0;
        while (i < s.length() - 1 && s.charAt(i) == '0') {
            i++;
        }
        return s.substring(i);
    }

    private static String[] splitStone(String s) {
        int mid = s.length() / 2;
        String left = trimLeadingZeros(s.substring(0, mid));
        String right = trimLeadingZeros(s.substring(mid));
        if (left.isEmpty()) {
            left = "0";
        }
        if (right.isEmpty()) {
            right = "0";
        }
        return new String[]{left, right};
    }

    private static String multiplyBy2024(String s) {
        BigInteger num = new BigInteger(s);
        BigInteger multiplier = new BigInteger("2024");
        return num.multiply(multiplier).toString();
    }

    public static void main(String[] args) {
        try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
            String line = br.readLine();
            if (line == null) {
                System.out.println("Input file is empty");
                return;
            }
            String[] stonesStr = line.split("\\s+");

            Map<String, Long> stonesMap = new HashMap<>();
            for (String s : stonesStr) {
                stonesMap.merge(s, 1L, Long::sum);
            }

            final int steps = 75;
            for (int step = 0; step < steps; step++) {
                Map<String, Long> newStonesMap = new HashMap<>();
                for (Map.Entry<String, Long> entry : stonesMap.entrySet()) {
                    String stone = entry.getKey();
                    long count = entry.getValue();
                    if (stone.equals("0")) {
                        newStonesMap.merge("1", count, Long::sum);
                    } else if (stone.length() % 2 == 0) {
                        String[] split = splitStone(stone);
                        newStonesMap.merge(split[0], count, Long::sum);
                        newStonesMap.merge(split[1], count, Long::sum);
                    } else {
                        String newStone = multiplyBy2024(stone);
                        newStonesMap.merge(newStone, count, Long::sum);
                    }
                }
                stonesMap = newStonesMap;
            }

            long totalStones = 0;
            for (long count : stonesMap.values()) {
                totalStones += count;
            }

            System.out.println(totalStones);

        } catch (IOException e) {
            System.out.println("Error reading input.txt: " + e.getMessage());
        }
    }
}
