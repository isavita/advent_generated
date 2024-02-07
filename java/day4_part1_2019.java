
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class solution {
    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            String rangeStr = reader.readLine();
            String[] parts = rangeStr.split("-");
            int start = Integer.parseInt(parts[0]);
            int end = Integer.parseInt(parts[1]);

            int count = 0;
            for (int i = start; i <= end; i++) {
                String s = Integer.toString(i);
                if (hasDoubleAndIncreasingDigits(s)) {
                    count++;
                }
            }

            System.out.println(count);

        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public static boolean hasDoubleAndIncreasingDigits(String s) {
        boolean hasDouble = false;
        for (int i = 0; i < s.length() - 1; i++) {
            if (s.charAt(i) == s.charAt(i + 1)) {
                hasDouble = true;
            }
            if (s.charAt(i) > s.charAt(i + 1)) {
                return false;
            }
        }
        return hasDouble;
    }
}
