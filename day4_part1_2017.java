
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashSet;
import java.util.Set;

public class Solution {
    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            String line;
            int validCount = 0;

            while ((line = reader.readLine()) != null) {
                String[] words = line.trim().split(" ");
                Set<String> wordSet = new HashSet<>();

                boolean valid = true;
                for (String word : words) {
                    if (wordSet.contains(word)) {
                        valid = false;
                        break;
                    }
                    wordSet.add(word);
                }

                if (valid) {
                    validCount++;
                }
            }

            System.out.println(validCount);

            reader.close();
        } catch (IOException e) {
            System.out.println("File reading error: " + e.getMessage());
        }
    }
}
