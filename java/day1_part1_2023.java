
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class solution {
    public static void main(String[] args) {
        int sum = 0;
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            String line;

            while ((line = reader.readLine()) != null) {
                if (line.isEmpty()) {
                    continue;
                }
                int firstDigit = -1;
                int lastDigit = -1;

                for (char c : line.toCharArray()) {
                    if (Character.isDigit(c)) {
                        if (firstDigit == -1) {
                            firstDigit = Character.getNumericValue(c);
                        }
                        lastDigit = Character.getNumericValue(c);
                    }
                }

                if (firstDigit != -1 && lastDigit != -1) {
                    int value = Integer.parseInt("" + firstDigit + lastDigit);
                    sum += value;
                }
            }

            reader.close();
        } catch (IOException e) {
            System.out.println("Error reading file: " + e.getMessage());
            return;
        }

        System.out.println(sum);
    }
}
