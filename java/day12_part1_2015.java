
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class JSONNumberSummer {

    public static void main(String[] args) {
        try (BufferedReader reader = new BufferedReader(new FileReader("input.txt"))) {
            String line;
            long sum = 0;
            while ((line = reader.readLine()) != null) {
                sum += sumNumbers(line);
            }
            System.out.println(sum);
        } catch (IOException e) {
            System.err.println("Error reading file: " + e.getMessage());
        }
    }

    private static long sumNumbers(String line) {
        long sum = 0;
        StringBuilder currentNumber = new StringBuilder();
        for (char c : line.toCharArray()) {
            if (Character.isDigit(c) || c == '-') {
                currentNumber.append(c);
            } else {
                if (currentNumber.length() > 0) {
                    sum += Long.parseLong(currentNumber.toString());
                    currentNumber.setLength(0);
                }
            }
        }
        if (currentNumber.length() > 0) {
            sum += Long.parseLong(currentNumber.toString());
        }
        return sum;
    }
}
