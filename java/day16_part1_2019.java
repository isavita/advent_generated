
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class solution {

    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            String input = reader.readLine();

            int[] digits = new int[input.length()];
            for (int i = 0; i < input.length(); i++) {
                digits[i] = Character.getNumericValue(input.charAt(i));
            }

            for (int phase = 0; phase < 100; phase++) {
                digits = applyFFT(digits);
            }

            for (int i = 0; i < 8; i++) {
                System.out.print(digits[i]);
            }
            System.out.println();

            reader.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public static int[] applyFFT(int[] input) {
        int[] basePattern = {0, 1, 0, -1};
        int[] output = new int[input.length];
        for (int i = 0; i < input.length; i++) {
            int sum = 0;
            for (int j = 0; j < input.length; j++) {
                int patternValue = basePattern[((j + 1) / (i + 1)) % basePattern.length];
                sum += input[j] * patternValue;
            }
            output[i] = Math.abs(sum % 10);
        }
        return output;
    }
}
