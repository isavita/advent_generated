
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class Solution {
    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            String line;
            int sum = 0;
            while ((line = reader.readLine()) != null) {
                int[] digits = findFirstAndLastDigit(line);
                sum += 10 * digits[0] + digits[1];
            }
            System.out.println(sum);
            reader.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public static int[] findFirstAndLastDigit(String line) {
        String[] digits = {"zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"};
        int firstDigit = 0;
        int lastDigit = 0;
        for (int i = 0; i < line.length(); i++) {
            char c = line.charAt(i);
            String digitStr = String.valueOf(c);
            if (Character.isDigit(c)) {
                if (firstDigit == 0) {
                    firstDigit = Character.getNumericValue(c);
                }
                lastDigit = Character.getNumericValue(c);
            } else {
                for (int j = 0; j < digits.length; j++) {
                    if (line.substring(i).startsWith(digits[j])) {
                        if (firstDigit == 0) {
                            firstDigit = j;
                        }
                        lastDigit = j;
                        break;
                    }
                }
            }
        }
        return new int[]{firstDigit, lastDigit};
    }
}
