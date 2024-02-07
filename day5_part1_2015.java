
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class Solution {
    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            String input = reader.readLine();
            int nice = 0;
            String[] disallow = {"ab", "cd", "pq", "xy"};
            while (input != null) {
                int vowels = 0;
                for (char c : input.toCharArray()) {
                    if ("aeiou".indexOf(c) != -1) {
                        vowels++;
                    }
                }
                boolean hasDouble = false;
                for (int i = 0; i < input.length() - 1; i++) {
                    if (input.charAt(i) == input.charAt(i + 1)) {
                        hasDouble = true;
                        break;
                    }
                }
                if (vowels >= 3 && !containsDisallow(input, disallow) && hasDouble) {
                    nice++;
                }
                input = reader.readLine();
            }
            System.out.println(nice);
            reader.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private static boolean containsDisallow(String input, String[] disallow) {
        for (String str : disallow) {
            if (input.contains(str)) {
                return true;
            }
        }
        return false;
    }
}
