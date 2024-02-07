
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class solution {
    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            String rangeStr = reader.readLine().trim();
            String[] ranges = rangeStr.split("-");
            int start = Integer.parseInt(ranges[0]);
            int end = Integer.parseInt(ranges[1]);

            int count = 0;
            for (int i = start; i <= end; i++) {
                if (isValidPassword(i)) {
                    count++;
                }
            }

            System.out.println(count);
            reader.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public static boolean isValidPassword(int password) {
        String s = String.valueOf(password);
        boolean hasDouble = false;

        for (int i = 0; i < s.length() - 1; i++) {
            if (s.charAt(i) > s.charAt(i + 1)) {
                return false;
            }
            if (s.charAt(i) == s.charAt(i + 1)) {
                if ((i == 0 || s.charAt(i) != s.charAt(i - 1)) && (i + 2 >= s.length() || s.charAt(i) != s.charAt(i + 2))) {
                    hasDouble = true;
                }
            }
        }

        return hasDouble;
    }
}
