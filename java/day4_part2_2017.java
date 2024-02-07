
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

public class solution {
    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            String line;
            int validCount = 0;

            while ((line = reader.readLine()) != null) {
                String[] passphrases = line.split(" ");

                Map<String, Boolean> wordSet = new HashMap<>();
                boolean valid = true;

                for (String word : passphrases) {
                    char[] chars = word.toCharArray();
                    Arrays.sort(chars);
                    String sortedWord = new String(chars);

                    if (wordSet.containsKey(sortedWord)) {
                        valid = false;
                        break;
                    }

                    wordSet.put(sortedWord, true);
                }

                if (valid) {
                    validCount++;
                }
            }

            System.out.println(validCount);

            reader.close();
        } catch (IOException e) {
            System.out.println("File reading error" + e);
        }
    }
}
