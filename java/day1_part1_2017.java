
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class Solution {
    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            String input = reader.readLine().trim();
            int sum = 0;

            for (int i = 0; i < input.length(); i++) {
                int next = (i + 1) % input.length();
                if (input.charAt(i) == input.charAt(next)) {
                    sum += input.charAt(i) - '0';
                }
            }

            System.out.println(sum);
            reader.close();
        } catch (IOException e) {
            System.out.println("File reading error " + e.getMessage());
        }
    }
}
