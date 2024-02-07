
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

public class solution {
    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            String line = reader.readLine();
            String[] startingNumbers = line.split(",");

            Map<Integer, Integer> lastSpoken = new HashMap<>();
            int lastNumber = 0;
            int nextNumber = 0;

            for (int turn = 1; turn <= 2020; turn++) {
                if (turn - 1 < startingNumbers.length) {
                    lastNumber = Integer.parseInt(startingNumbers[turn - 1]);
                    lastSpoken.put(lastNumber, turn);
                    continue;
                }
                if (lastSpoken.containsKey(lastNumber) && lastSpoken.get(lastNumber) != turn - 1) {
                    nextNumber = turn - 1 - lastSpoken.get(lastNumber);
                } else {
                    nextNumber = 0;
                }
                lastSpoken.put(lastNumber, turn - 1);
                lastNumber = nextNumber;
            }

            System.out.println(lastNumber);
        } catch (IOException e) {
            System.out.println("Error reading file: " + e.getMessage());
        }
    }
}
