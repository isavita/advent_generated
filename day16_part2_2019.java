
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class solution {
    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            String input = reader.readLine();
            reader.close();

            int[] repeatedInput = repeatInput(input, 10000);

            int offset = Integer.parseInt(input.substring(0, 7));

            for (int phase = 0; phase < 100; phase++) {
                int sum = 0;
                for (int i = repeatedInput.length - 1; i >= offset; i--) {
                    sum += repeatedInput[i];
                    repeatedInput[i] = sum % 10;
                }
            }

            for (int i = offset; i < offset + 8; i++) {
                System.out.print(repeatedInput[i]);
            }
            System.out.println();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public static int[] repeatInput(String input, int times) {
        int[] digits = new int[input.length() * times];
        for (int t = 0; t < times; t++) {
            for (int i = 0; i < input.length(); i++) {
                int digit = Integer.parseInt(String.valueOf(input.charAt(i)));
                digits[t * input.length() + i] = digit;
            }
        }
        return digits;
    }
}
